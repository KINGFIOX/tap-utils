{-# LANGUAGE ForeignFunctionInterface #-}

module Network.TapUtils
  ( allocTap,
    tapGet,
    tapPut,
    tapGetBuf,
    tapPutBuf,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString, packCStringLen, useAsCStringLen)
import Foreign
import Foreign.C.Error (Errno (Errno), errnoToIOError)
import Foreign.C.String
import Foreign.C.Types
import GHC.IO.Handle (Handle)
import Network.Socket (HostAddress)
import System.Posix.IO (fdToHandle)
import System.Posix.IO.ByteString (handleToFd)

-- | needs to set the device previously
-- | ip tuntap add mode tap dev tap0 user wangfiox group wheel
-- | ip addr add 10.0.0.1/24 dev tap0
-- | ip link set tap0 up
allocTap :: String -> IO (Handle, [Word8], HostAddress, Int)
allocTap devName = withCString devName $ \namePtr -> do
  -- Allocate memory for the required parameters
  allocaBytes 6 $ \hwaddrPtr ->
    alloca $ \ipaddrPtr ->
      alloca $ \mtuPtr -> do
        ret <- c_alloc_tap namePtr hwaddrPtr ipaddrPtr mtuPtr
        if ret < 0
          then ioError (errnoToIOError "allocTap" (Errno (negate ret)) Nothing Nothing)
          else do
            mac <- fmap (fmap fromIntegral) (peekArray 6 hwaddrPtr)
            ipaddr <- fromIntegral <$> peek ipaddrPtr
            mtu <- fromIntegral <$> peek mtuPtr
            handle <- fdToHandle (fromIntegral ret)
            return (handle, mac, ipaddr, mtu)

tapPut :: Handle -> ByteString -> IO ()
tapPut handle bs = do
  useAsCStringLen bs $ \(src, len) -> do
    tapPutBuf handle src len

tapGet :: Handle -> Int -> IO ByteString
tapGet handle len = do
  buf <- mallocBytes len
  ret <- tapGetBuf handle buf len
  packCStringLen (castPtr buf, ret)

tapGetBuf :: Handle -> Ptr a -> Int -> IO Int
tapGetBuf handle buf len = do
  fd <- handleToFd handle
  rc <- c_tap_read (fromIntegral fd) (castPtr buf) (fromIntegral len)
  if rc < 0
    then ioError (errnoToIOError "tapGetBuf" (Errno (negate rc)) Nothing Nothing)
    else return (fromIntegral rc)

tapPutBuf :: Handle -> Ptr a -> Int -> IO ()
tapPutBuf handle buf len = do
  fd <- handleToFd handle
  rc <- c_tap_write (fromIntegral fd) (castPtr buf) (fromIntegral len)
  when (rc < 0) $ ioError (errnoToIOError "tapPutBuf" (Errno (negate rc)) Nothing Nothing)

foreign import ccall unsafe "alloc_tap"
  c_alloc_tap :: CString -> Ptr CUChar -> Ptr CUInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "tap_read"
  c_tap_read :: CInt -> Ptr CUChar -> CInt -> IO CInt

foreign import ccall unsafe "tap_write"
  c_tap_write :: CInt -> Ptr CUChar -> CInt -> IO CInt
