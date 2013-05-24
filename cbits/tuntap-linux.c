#include <netinet/ip.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/if_tun.h>
#include <stdio.h>
#include <string.h>

static int tuncount = 0;

int open_tun(char *name)
{
    int fd = open("/dev/net/tun", O_RDWR);
    if (fd < 0) {
        perror("open");
        return -1;
    }

    snprintf(name, sizeof(name)-1, "tun%d", tuncount);
    tuncount++;

    struct ifreq ifr;
    strncpy(ifr.ifr_name, name, IFNAMSIZ);
    ifr.ifr_flags = IFF_TUN | IFF_NO_PI;
    if (ioctl(fd, TUNSETIFF, &ifr) < 0) {
        perror("ioctl");
        return -1;
    }
    return fd;
}
