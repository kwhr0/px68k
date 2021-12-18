// -----------------------------------------------------------------------
//   55.6fpsキープ用たいまー
// -----------------------------------------------------------------------
#include "common.h"
#include "crtc.h"
#include "mfp.h"
#include <unistd.h>

DWORD	timercnt = 0;
DWORD	tick = 0;

void Timer_Init(void)
{
	tick = timeGetTime();
}

void Timer_Reset(void)
{
	tick = timeGetTime();
}

WORD Timer_GetCount(void)
{
	DWORD ticknow = timeGetTime();
	DWORD dif = ticknow-tick;
	DWORD TIMEBASE = ((CRTC_Regs[0x29]&0x10)?VSYNC_HIGH:VSYNC_NORM);

	timercnt += dif*10000;
	tick = ticknow;
	if ( timercnt>=TIMEBASE ) {
//		timercnt = 0;
		timercnt -= TIMEBASE;
		if ( timercnt>=(TIMEBASE*2) ) timercnt = 0;
		return 1;
	} else {
#if 1
		usleep((TIMEBASE - timercnt) / 10);
		timercnt = 0;
		tick = timeGetTime();
		return 1;
#else
		return 0;
#endif
	}
}
