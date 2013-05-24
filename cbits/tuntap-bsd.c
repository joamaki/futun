#include <netinet/ip.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

int open_tun(char *name)
{
    char path[48];
    if (strncmp(name, "tun", 3) != 0) {
        fprintf(stderr, "open_tun: Device name must start with 'tun'!\n");
	return -1;
    }
    snprintf(path, sizeof(path)-1, "/dev/%s", name);

    int fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "open_tun: Error opening %s: %s\n",
	    name, strerror(errno));
        return -1;
    }
    return fd;
}
