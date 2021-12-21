#include <sys/iocs.h>
#include <sys/dos.h>

#define MOUSE	(*(volatile unsigned char *)0xe8e019)

void main(void) {
	while (1) {
		int x, y;
		MOUSE = 0;
		x = MOUSE;
		x |= MOUSE << 8;
		y = MOUSE;
		y |= MOUSE << 8;
		_iocs_ms_curst(x, y);
		_dos_sleep_pr(16);
	}
}
