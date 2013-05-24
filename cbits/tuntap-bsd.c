#include <netinet/ip.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <stdio.h>
#include <string.h>

static int tuncount = 0;

int open_tun(char *name)
{
    char path[48];
    snprintf(name, sizeof(name)-1, "tun%d", tuncount);
    snprintf(path, sizeof(path)-1, "/dev/%s", name);

    tuncount++;
    int fd = open(path, O_RDWR);
    if (fd < 0) {
        perror("open");
        return -1;
    }
    return fd;
}
