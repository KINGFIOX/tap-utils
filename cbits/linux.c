#include <fcntl.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include <linux/if_tun.h>
#include <linux/in.h>
#include <linux/socket.h>
#include <net/if.h>
#include <pthread.h>

#define ETH_ALEN 6

static inline int get_skfd(void) {
  static int skfd = -1;
  static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
  pthread_mutex_lock(&mutex);
  if (skfd == -1) {
    skfd = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
    if (skfd < 0) { // err
      return skfd;
    }
  }
  pthread_mutex_unlock(&mutex);
  return skfd;
}

static inline int getmtu_tap(const char *name, int *mtu) {
  int ret;
  struct ifreq ifr = {};

  int skfd = get_skfd();
  if (skfd < 0) { // err
    return skfd;
  }

  strcpy(ifr.ifr_name, (char *)name);
  /* get net order hardware address */
  if ((ret = ioctl(skfd, SIOCGIFMTU, (void *)&ifr)) < 0) {
    return ret;
  }
  *mtu = ifr.ifr_mtu;
  return 0;
}

static inline int getipaddr_tap(const char *name, unsigned int *ipaddr) {
  int ret;
  struct ifreq ifr = {};
  struct sockaddr_in *saddr;
  int skfd = get_skfd();
  if (skfd < 0) { // err
    return skfd;
  }

  strcpy(ifr.ifr_name, (char *)name);
  if ((ret = ioctl(skfd, SIOCGIFADDR, (void *)&ifr)) < 0) {
    return ret;
  }
  saddr = (struct sockaddr_in *)&ifr.ifr_addr;
  *ipaddr = saddr->sin_addr.s_addr;
  return 0;
}

static inline int gethwaddr_tap(int tapfd, unsigned char *mac) {
  int ret;
  struct ifreq ifr = {};
  /* get net order hardware address */
  if ((ret = ioctl(tapfd, SIOCGIFHWADDR, (void *)&ifr) < 0)) {
    return ret;
  }
  memcpy(mac, ifr.ifr_hwaddr.sa_data, ETH_ALEN);
  return 0;
}

int alloc_tap(const char *dev, unsigned char *mac, unsigned int *ipaddr,
              int *mtu) {
  int rc;
  struct ifreq ifr = {};

  int tapfd = open("/dev/net/tun", O_RDWR);

  ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
  if (*dev) {
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);
  }
  /*
   * create a new tap device
   * if created already, just bind tun with file
   */
  if ((rc = ioctl(tapfd, TUNSETIFF, (void *)&ifr) < 0)) {
    return rc;
  }
  if ((rc = gethwaddr_tap(tapfd, mac)) < 0) {
    return rc;
  }
  if ((rc = getipaddr_tap(dev, ipaddr)) < 0) {
    return rc;
  }
  if ((rc = getmtu_tap(dev, mtu)) < 0) {
    return rc;
  }

  return tapfd;
}

int tap_read(int tapfd, unsigned char *buf, int len) {
  return read(tapfd, buf, len);
}

int tap_write(int tapfd, unsigned char *buf, int len) {
  return write(tapfd, buf, len);
}
