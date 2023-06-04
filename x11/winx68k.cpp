#include "tiny68000.h"
#include "test.h"

#if defined(TESTLOG) || defined(CURMPU) && defined(NEWMPU)
#define STEP		1
#else
#define STEP		n
#endif

//#define MOUSE_GRAB

#ifdef MOUSE_GRAB
#define GRAB(f)	SDL_SetRelativeMouseMode(f)
#else
#define GRAB(f)
#endif

M68000 m68000;
void newirq(int level) {
	m68000.IRQ(level);
}
unsigned newgetpc(void) {
	return m68000.GetPC();
}

#ifdef  __cplusplus
extern "C" {
#endif 

#include <SDL.h>
#ifdef USE_OGLES11
#include <SDL_opengles.h>
#endif
#ifdef CURMPU
#include "../m68000/m68000.h"
#endif
#include "common.h"
#include "fileio.h"
#include "timer.h"
#include "keyboard.h"
#include "prop.h"
#include "status.h"
#include "joystick.h"
#include "mkcgrom.h"
#include "winx68k.h"
#include "windraw.h"
#include "winui.h"
#include "../x68k/memory.h"
#include "mfp.h"
#include "opm.h"
#include "bg.h"
#include "adpcm.h"
#include "mercury.h"
#include "crtc.h"
#include "mfp.h"
#include "fdc.h"
#include "fdd.h"
#include "dmac.h"
#include "irqh.h"
#include "ioc.h"
#include "rtc.h"
#include "sasi.h"
#include "scsi.h"
#include "sysport.h"
#include "bg.h"
#include "palette.h"
#include "crtc.h"
#include "pia.h"
#include "scc.h"
#include "midi.h"
#include "sram.h"
#include "gvram.h"
#include "tvram.h"
#include "mouse.h"

#include "dswin.h"
#include "fmg_wrap.h"

#include "irqh.h"

#ifdef RFMDRV
int rfd_sock;
#endif

int m68000_ICountBk;
int ICount;

FILE *testlog;

//#include "../icons/keropi_mono.xbm"

#define	APPNAME	"Keropi"

extern	WORD	BG_CHREND;
extern	WORD	BG_BGTOP;
extern	WORD	BG_BGEND;
extern	BYTE	BG_CHRSIZE;

#if defined(ANDROID) || TARGET_OS_IPHONE
extern SDL_TouchID touchId;
#endif

const	BYTE	PrgName[] = "Keropi";
const	BYTE	PrgTitle[] = APPNAME;

char	winx68k_dir[MAX_PATH];
char	winx68k_ini[MAX_PATH];

WORD	VLINE_TOTAL = 567;
DWORD	VLINE = 0;
DWORD	vline = 0;

extern	int	SplashFlag;

BYTE DispFrame = 0;
DWORD SoundSampleRate;

unsigned int hTimerID = 0;
DWORD TimerICount = 0;
extern DWORD timertick;

BYTE ForceDebugMode = 0;
DWORD skippedframes = 0;

static int ClkUsed = 0;
static int FrameSkipCount = 0;
static int FrameSkipQueue = 0;

#ifdef __cplusplus
};
#endif

#if SDL_VERSION_ATLEAST(2, 0, 0)
SDL_Window *sdl_window;
int realdisp_w, realdisp_h;
#endif

void
WinX68k_SCSICheck(void)
{
	static const BYTE SCSIIMG[] = {
		0x00, 0xfc, 0x00, 0x14,			// $fc0000 SCSI起動用のエントリアドレス
		0x00, 0xfc, 0x00, 0x16,			// $fc0004 IOCSベクタ設定のエントリアドレス(必ず"Human"の8バイト前)
		0x00, 0x00, 0x00, 0x00,			// $fc0008 ?
		0x48, 0x75, 0x6d, 0x61,			// $fc000c ↓
		0x6e, 0x36, 0x38, 0x6b,			// $fc0010 ID "Human68k"	(必ず起動エントリポイントの直前)
		0x4e, 0x75,				// $fc0014 "rts"		(起動エントリポイント)
		0x23, 0xfc, 0x00, 0xfc, 0x00, 0x2a,	// $fc0016 ↓		(IOCSベクタ設定エントリポイント)
		0x00, 0x00, 0x07, 0xd4,			// $fc001c "move.l #$fc002a, $7d4.l"
		0x74, 0xff,				// $fc0020 "moveq #-1, d2"
		0x4e, 0x75,				// $fc0022 "rts"
//		0x53, 0x43, 0x53, 0x49, 0x49, 0x4e,	// $fc0024 ID "SCSIIN"
// 内蔵SCSIをONにすると、SASIは自動的にOFFになっちゃうらしい…
// よって、IDはマッチしないようにしておく…
		0x44, 0x55, 0x4d, 0x4d, 0x59, 0x20,	// $fc0024 ID "DUMMY "
		0x70, 0xff,				// $fc002a "moveq #-1, d0"	(SCSI IOCSコールエントリポイント)
		0x4e, 0x75,				// $fc002c "rts"
	};

#if 0
	DWORD *p;
#endif
	WORD *p1, *p2;
	int scsi;
	int i;

	scsi = 0;
	for (i = 0x30600; i < 0x30c00; i += 2) {
#if 0 // 4の倍数ではない偶数アドレスからの4バイト長アクセスはMIPSには無理
		p = (DWORD *)(&IPL[i]);
		if (*p == 0x0000fc00)
			scsi = 1;
#else
		p1 = (WORD *)(&IPL[i]);
		p2 = p1 + 1;
		// xxx: works only for little endian guys
		if (*p1 == 0xfc00 && *p2 == 0x0000) {
			scsi = 1;
			break;
		}
#endif
	}

	// SCSIモデルのとき
	if (scsi) {
		ZeroMemory(IPL, 0x2000);		// 本体は8kb
		memset(&IPL[0x2000], 0xff, 0x1e000);	// 残りは0xff
		memcpy(IPL, SCSIIMG, sizeof(SCSIIMG));	// インチキSCSI BIOS
//		Memory_SetSCSIMode();
	} else {
		// SASIモデルはIPLがそのまま見える
		memcpy(IPL, &IPL[0x20000], 0x20000);
	}
}

int
WinX68k_LoadROMs(void)
{
	static const char *BIOSFILE[] = {
		"iplrom.dat", "iplrom30.dat", "iplromco.dat", "iplromxv.dat"
	};
	static const char FONTFILE[] = "cgrom.dat";
	static const char FONTFILETMP[] = "cgrom.tmp";
	FILEH fp;
	int i;
	BYTE tmp;

	for (fp = 0, i = 0; fp == 0 && i < NELEMENTS(BIOSFILE); ++i) {
		fp = File_OpenCurDir((char *)BIOSFILE[i]);
	}

	if (fp == 0) {
		Error("BIOS ROM イメージが見つかりません.");
		return FALSE;
	}

	File_Read(fp, &IPL[0x20000], 0x20000);
	File_Close(fp);

	WinX68k_SCSICheck();	// SCSI IPLなら、$fc0000〜にSCSI BIOSを置く

	for (i = 0; i < 0x40000; i += 2) {
		tmp = IPL[i];
		IPL[i] = IPL[i + 1];
		IPL[i + 1] = tmp;
	}

	fp = File_OpenCurDir((char *)FONTFILE);
	if (fp == 0) {
		// cgrom.tmpがある？
		fp = File_OpenCurDir((char *)FONTFILETMP);
		if (fp == 0) {
#if 1
			// フォント生成 XXX
			printf("フォントROMイメージが見つかりません\n");
			return FALSE;
#else
			MessageBox(hWndMain,
				"フォントROMイメージが見つかりません.\nWindowsフォントから新規に作成します.",
				"けろぴーのメッセージ", MB_ICONWARNING | MB_OK);
			SSTP_SendMes(SSTPMES_MAKEFONT);
			make_cgromdat(FONT, FALSE, "ＭＳ ゴシック", "ＭＳ 明朝");
			//WinX68k_MakeFont();
			//DialogBox(hInst, MAKEINTRESOURCE(IDD_PROGBAR),
			//		hWndMain, (DLGPROC)MakeFontProc);
			fp = File_CreateCurDir(FONTFILETMP);
			if (fp)
			{
				File_Write(fp, FONT, 0xc0000);
				File_Close(fp);
				return TRUE;
			}
			return TRUE;
#endif
		}
	}
	File_Read(fp, FONT, 0xc0000);
	File_Close(fp);

	return TRUE;
}

int
WinX68k_Reset(void)
{
	OPM_Reset();
#ifdef NEWMPU
	(int &)MEM[0] = (IPL[0x30003]<<24)|(IPL[0x30002]<<16)|(IPL[0x30001]<<8)|IPL[0x30000];
	(int &)MEM[4] = (IPL[0x30007]<<24)|(IPL[0x30006]<<16)|(IPL[0x30005]<<8)|IPL[0x30004];
	m68000.Reset();
#endif
#ifdef CURMPU
	C68k_Reset(&C68K);
	C68k_Set_AReg(&C68K, 7, (IPL[0x30001]<<24)|(IPL[0x30000]<<16)|(IPL[0x30003]<<8)|IPL[0x30002]);
	C68k_Set_PC(&C68K, (IPL[0x30005]<<24)|(IPL[0x30004]<<16)|(IPL[0x30007]<<8)|IPL[0x30006]);
#endif
	Memory_Init();
	CRTC_Init();
	DMA_Init();
	MFP_Init();
	FDC_Init();
	FDD_Reset();
	SASI_Init();
	SCSI_Init();
	IOC_Init();
	SCC_Init();
	PIA_Init();
	RTC_Init();
	TVRAM_Init();
	GVRAM_Init();
	BG_Init();
	Pal_Init();
	IRQH_Init();
	MIDI_Init();
	//WinDrv_Init();

	m68000_ICountBk = 0;
	ICount = 0;

	DSound_Stop();
	SRAM_VirusCheck();
	//CDROM_Init();
	DSound_Play();

	return TRUE;
}


int
WinX68k_Init(void)
{
#ifdef PSP
#define MEM_SIZE 0x400000
#else
#define MEM_SIZE 0xc00000
#endif
	MEM = (BYTE*)malloc(0x1000000); // 16MB
	IPL = MEM + 0xfc0000;
	FONT = MEM + 0xf00000;

	if (MEM)
		ZeroMemory(MEM, MEM_SIZE);

	if (MEM && FONT && IPL) {
#ifdef NEWMPU
		m68000.SetMemoryPtr(MEM);
		m68000.SetIntrVecFunc(my_irqh_callback);
#if defined(TESTLOG) || defined(CURMPU) && defined(NEWMPU)
		m68000.SetIORange32(0, 0x1000000);
#else
		m68000.SetIORange32(0xc00000, 0x1000000);
#endif
#endif
#ifdef CURMPU
	  	m68000_init();
#endif
#ifdef TESTLOG
		char path[256];
		sprintf(path, "%s/Desktop/testlog.txt", getenv("HOME"));
		testlog = fopen(path, "w");
#endif
		return TRUE;
	} else
		return FALSE;
}

void
WinX68k_Cleanup(void)
{
	if (MEM) {
		free(MEM);
		MEM = 0;
	}
	if (testlog) fclose(testlog);
}

#define CLOCK_SLICE 200
// -----------------------------------------------------------------------------------
//  コアのめいんるーぷ
// -----------------------------------------------------------------------------------
void WinX68k_Exec(void)
{
	//char *test = NULL;
	int clk_total, clkdiv, usedclk, hsync, clk_next, clk_count, clk_line=0;
	int KeyIntCnt = 0, MouseIntCnt = 0;
	DWORD t_start = timeGetTime(), t_end;

	if ( Config.FrameRate != 7 ) {
		DispFrame = (DispFrame+1)%Config.FrameRate;
	} else {				// Auto Frame Skip
		if ( FrameSkipQueue ) {
			if ( FrameSkipCount>15 ) {
				FrameSkipCount = 0;
				FrameSkipQueue++;
				DispFrame = 0;
			} else {
				FrameSkipCount++;
				FrameSkipQueue--;
				DispFrame = 1;
			}
		} else {
			FrameSkipCount = 0;
			DispFrame = 0;
		}
	}

	vline = 0;
	clk_count = -ICount;
	clk_total = (CRTC_Regs[0x29] & 0x10) ? VSYNC_HIGH : VSYNC_NORM;
	if (Config.XVIMode == 1) {
		clk_total = (clk_total*16)/10;
		clkdiv = 16;
	} else if (Config.XVIMode == 2) {
		clk_total = (clk_total*24)/10;
		clkdiv = 24;
	} else {
		clkdiv = 10;
	}
	ICount += clk_total;
	clk_next = (clk_total/VLINE_TOTAL);
	hsync = 1;

	do {
		int m, n = (ICount>CLOCK_SLICE)?CLOCK_SLICE:ICount;
		if ( hsync ) {
			hsync = 0;
			clk_line = 0;
			MFP_Int(0);
			if ( (vline>=CRTC_VSTART)&&(vline<CRTC_VEND) )
				VLINE = ((vline-CRTC_VSTART)*CRTC_VStep)/2;
			else
				VLINE = (DWORD)-1;
			if ( (!(MFP[MFP_AER]&0x40))&&(vline==CRTC_IntLine) )
				MFP_Int(1);
			if ( MFP[MFP_AER]&0x10 ) {
				if ( vline==CRTC_VSTART )
					MFP_Int(9);
			} else {
				if ( CRTC_VEND>=VLINE_TOTAL ) {
					if ( (long)vline==(CRTC_VEND-VLINE_TOTAL) ) MFP_Int(9);		// エキサイティングアワーとか（TOTAL<VEND）
				} else {
					if ( (long)vline==(VLINE_TOTAL-1) ) MFP_Int(9);			// クレイジークライマーはコレでないとダメ？
				}
			}
		}

		{
			extern void setCompare(int f);
#ifdef CURMPU
			if (testlog) fprintf(testlog, "\n%06x ", C68K.PC-C68K.BasePC);
#ifdef NEWMPU
			setCompare(false);
#endif
			C68k_Exec(&C68K, STEP);
#endif
#ifdef NEWMPU
			if (testlog) fprintf(testlog, "\n%06x ", newgetpc());
#ifdef CURMPU
			setCompare(true);
#endif
			m68000.Execute(STEP);
#endif
			m = (n-m68000_ICountBk);			// 経過クロック数
			ClkUsed += m*10;
			usedclk = ClkUsed/clkdiv;
			clk_line += usedclk;
			ClkUsed -= usedclk*clkdiv;
			ICount -= m;
			clk_count += m;
		}

		MFP_Timer(usedclk);
		RTC_Timer(usedclk);
		DMA_Exec(0);
		DMA_Exec(1);
		DMA_Exec(2);

		if ( clk_count>=clk_next ) {
			//OPM_RomeoOut(Config.BufferSize*5);
			//MIDI_DelayOut((Config.MIDIAutoDelay)?(Config.BufferSize*5):Config.MIDIDelay);
			MFP_TimerA();
			if ( (MFP[MFP_AER]&0x40)&&(vline==CRTC_IntLine) )
				MFP_Int(1);
			if ( (!DispFrame)&&(vline>=CRTC_VSTART)&&(vline<CRTC_VEND) ) {
				if ( CRTC_VStep==1 ) {				// HighReso 256dot（2度読み）
					if ( vline%2 )
						WinDraw_DrawLine();
				} else if ( CRTC_VStep==4 ) {		// LowReso 512dot
					WinDraw_DrawLine();				// 1走査線で2回描く（インターレース）
					VLINE++;
					WinDraw_DrawLine();
				} else {							// High 512dot / Low 256dot
					WinDraw_DrawLine();
				}
			}

			ADPCM_PreUpdate(clk_line);
			OPM_Timer(clk_line);
			MIDI_Timer(clk_line);
#ifndef	NO_MERCURY
			Mcry_PreUpdate(clk_line);
#endif

			KeyIntCnt++;
			if ( KeyIntCnt>(VLINE_TOTAL/4) ) {
				KeyIntCnt = 0;
				Keyboard_Int();
			}
			MouseIntCnt++;
			if ( MouseIntCnt>(VLINE_TOTAL/8) ) {
				MouseIntCnt = 0;
				SCC_IntCheck();
			}
			DSound_Send0(clk_line);

			vline++;
			clk_next  = (clk_total*(vline+1))/VLINE_TOTAL;
			hsync = 1;
		}
	} while ( vline<VLINE_TOTAL );

	if ( CRTC_Mode&2 ) {		// FastClrビットの調整（PITAPAT）
		if ( CRTC_FastClr ) {	// FastClr=1 且つ CRTC_Mode&2 なら 終了
			CRTC_FastClr--;
			if ( !CRTC_FastClr )
				CRTC_Mode &= 0xfd;
		} else {				// FastClr開始
			if ( CRTC_Regs[0x29]&0x10 )
				CRTC_FastClr = 1;
			else
				CRTC_FastClr = 2;
			TVRAM_SetAllDirty();
			GVRAM_FastClear();
		}
	}

#ifdef PSP
	Joystick_Update(FALSE);
#else
	Joystick_Update(FALSE, SDLK_UNKNOWN);
#endif
	FDD_SetFDInt();
	if ( !DispFrame )
		WinDraw_Draw();
	TimerICount += clk_total;

	t_end = timeGetTime();
	if ( (int)(t_end-t_start)>((CRTC_Regs[0x29]&0x10)?14:16) ) {
		FrameSkipQueue += ((t_end-t_start)/((CRTC_Regs[0x29]&0x10)?14:16))+1;
		if ( FrameSkipQueue>100 )
			FrameSkipQueue = 100;
	}
}

//
// main
//
#ifdef PSP

#include <pspmoduleinfo.h>
#include <psppower.h>
#include <pspctrl.h>
#include <pspkernel.h>
#include <pspgu.h>

int exit_flag = 0;

int exit_callback(int arg1, int arg2, void *common)
{
	exit_flag = 1;

	return 0;
}

int CallbackThread(SceSize args, void *argp)
{
	int cbid;

	cbid = sceKernelCreateCallback("Exit Callback", exit_callback, NULL);
	sceKernelRegisterExitCallback(cbid); //SetExitCallback(cbid);

	sceKernelSleepThreadCB(); //KernelPollCallbacks();

	return 0;
}

int SetupCallbacks(void)
{
	int thid;

	thid = sceKernelCreateThread("update_thread", CallbackThread,
				     0x11, 0xFA0, 0, 0);
	if (thid >= 0) {
		sceKernelStartThread(thid, 0, 0);
	}

	return thid;
}

PSP_HEAP_SIZE_KB(-1024);

extern "C" int
SDL_main(int argc, char *argv[])
#else
int main(int argc, char *argv[])
#endif
{
#ifndef PSP
	SDL_Event ev;
	SDL_Keycode menu_key_down;
#endif
#if defined(ANDROID) || TARGET_OS_IPHONE
	int vk_cnt = -1;
	int menu_cnt = -1;
	BYTE state;
#endif
	int sdlaudio = -1;
	enum {menu_out, menu_enter, menu_in};
	int menu_mode = menu_out;

#ifdef PSP
	SetupCallbacks();
	scePowerSetClockFrequency(333, 333, 166);

	sceCtrlSetSamplingCycle(0);
	sceCtrlSetSamplingMode(PSP_CTRL_MODE_ANALOG);
#endif

	p6logd("PX68K Ver.%s\n", PX68KVERSTR);

#ifdef RFMDRV
	struct sockaddr_in dest;

	memset(&dest, 0, sizeof(dest));
	dest.sin_port = htons(2151);
	dest.sin_family = AF_INET;
	dest.sin_addr.s_addr = inet_addr("127.0.0.1");

	rfd_sock = socket(AF_INET, SOCK_STREAM, 0);
	connect (rfd_sock, (struct sockaddr *)&dest, sizeof(dest));
#endif

	if (set_modulepath(winx68k_dir, sizeof(winx68k_dir)))
		return 1;

	dosio_init();
	file_setcd(winx68k_dir);
    puts(winx68k_dir);

	LoadConfig();

#ifndef NOSOUND
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) < 0) {
		p6logd("SDL_Init error\n");		
#endif
		if (SDL_Init(SDL_INIT_VIDEO) < 0) {
			return 1;
		}
#ifndef NOSOUND
	} else {
		sdlaudio = 0;
	}
#endif

#if !SDL_VERSION_ATLEAST(2, 0, 0)
	SDL_WM_SetCaption(APPNAME" SDL", NULL);
#ifndef PSP
        if (SDL_SetVideoMode(FULLSCREEN_WIDTH, FULLSCREEN_HEIGHT, 16, SDL_SWSURFACE) == NULL) {
#else
        if (SDL_SetVideoMode(480, 272, 16, SDL_SWSURFACE) == NULL) {
#endif
		puts("SDL_SetVideoMode() failed");
		return 1;
	}
#else
#ifdef USE_OGLES11
	SDL_DisplayMode sdl_dispmode;
	SDL_GetCurrentDisplayMode(0, &sdl_dispmode);
	p6logd("width: %d height: %d", sdl_dispmode.w, sdl_dispmode.h);
	// ナビゲーションバーを除くアプリが触れる画面
	realdisp_w = sdl_dispmode.w, realdisp_h = sdl_dispmode.h;

	SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 1 );
	SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 1 );

	SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 ); 

#if TARGET_OS_IPHONE
	sdl_window = SDL_CreateWindow(APPNAME" SDL", 0, 0, FULLSCREEN_WIDTH, FULLSCREEN_HEIGHT, SDL_WINDOW_OPENGL|SDL_WINDOW_SHOWN|SDL_WINDOW_BORDERLESS);
#else
	// for Android: window sizeの指定は関係なくフルスクリーンになるみたい
	sdl_window = SDL_CreateWindow(APPNAME" SDL", 0, 0, FULLSCREEN_WIDTH, FULLSCREEN_HEIGHT, SDL_WINDOW_OPENGL|SDL_WINDOW_SHOWN);
#endif
#else
	sdl_window = SDL_CreateWindow(APPNAME" SDL", 0, 0, FULLSCREEN_WIDTH, FULLSCREEN_HEIGHT, SDL_WINDOW_SHOWN);
#endif
	if (sdl_window == NULL) {
		p6logd("sdl_window: %ld", sdl_window);
	}

#ifdef USE_OGLES11
	SDL_GLContext glcontext = SDL_GL_CreateContext(sdl_window);

        glClearColor( 0, 0, 0, 0 );

	glEnable(GL_TEXTURE_2D);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	//glViewport(0, 0, 800, 600); //ここを増やさないとOpenGLの画面はせまい
	glViewport(0, 0, sdl_dispmode.w, sdl_dispmode.h);
	// スマホやタブの実画面に関係なくOpenGLの描画領域を800x600とする。
	// 800x600にした意味は特にない。
	glOrthof(0, 800, 600, 0, -1, 1);
	//  glOrthof(0, 1024, 0, 1024, -1, 1);
	glMatrixMode(GL_MODELVIEW);
#endif
#endif // !SDL_VERSION_ATLEAST(2, 0, 0)

	if (!WinDraw_MenuInit()) {
		WinX68k_Cleanup();
		WinDraw_Cleanup();
		return 1;
	}

	SplashFlag = 20;
	SoundSampleRate = Config.SampleRate;

	StatBar_Show(Config.WindowFDDStat);
	WinDraw_ChangeSize();
	WinDraw_ChangeMode(FALSE);

	WinUI_Init();
	WinDraw_StartupScreen();

	if (!WinX68k_Init()) {
		WinX68k_Cleanup();
		WinDraw_Cleanup();
		return 1;
	}

	if (!WinX68k_LoadROMs()) {
		WinX68k_Cleanup();
		WinDraw_Cleanup();
		exit (1);
	}

	Keyboard_Init(); //WinDraw_Init()前に移動

	if (!WinDraw_Init()) {
		WinDraw_Cleanup();
		Error("Error: Can't init screen.\n");
		return 1;
	}

	if ( SoundSampleRate ) {
		ADPCM_Init(SoundSampleRate);
		OPM_Init(4000000/*3579545*/, SoundSampleRate);
#ifndef	NO_MERCURY
		Mcry_Init(SoundSampleRate, winx68k_dir);
#endif
	} else {
		ADPCM_Init(100);
		OPM_Init(4000000/*3579545*/, 100);
#ifndef	NO_MERCURY
		Mcry_Init(100, winx68k_dir);
#endif
	}

	FDD_Init();
	SysPort_Init();
	Mouse_Init();
	Joystick_Init();
	SRAM_Init();
	WinX68k_Reset();
	Timer_Init();

	MIDI_Init();
	MIDI_SetMimpiMap(Config.ToneMapFile);	// 音色設定ファイル使用反映
	MIDI_EnableMimpiDef(Config.ToneMap);

	if (sdlaudio == 0 && !DSound_Init(Config.SampleRate, Config.BufferSize)) {
		if (Config.DSAlert)
			fprintf(stderr, "Can't init sound.\n");
	}

	ADPCM_SetVolume((BYTE)Config.PCM_VOL);
	OPM_SetVolume((BYTE)Config.OPM_VOL);
#ifndef	NO_MERCURY
	Mcry_SetVolume((BYTE)Config.MCR_VOL);
#endif
	DSound_Play();

	// command line から指定した場合
	switch (argc) {
	case 3:
		strcpy(Config.FDDImage[1], argv[2]);
	case 2:
		strcpy(Config.FDDImage[0], argv[1]);
		break;
	}

	FDD_SetFD(0, Config.FDDImage[0], 0);
	FDD_SetFD(1, Config.FDDImage[1], 0);

	//SDL_StartTextInput();
	GRAB(SDL_TRUE);
	while (1) {
		// OPM_RomeoOut(Config.BufferSize * 5);
		if (menu_mode == menu_out
		    && (Config.NoWaitMode || Timer_GetCount())) {
			WinX68k_Exec();
#if defined(ANDROID) || TARGET_OS_IPHONE
			if (vk_cnt > 0) {
				vk_cnt--;
				if (vk_cnt == 0) {
					p6logd("vk_cnt 0");
				}
			}
			if  (menu_cnt > 0) {
				menu_cnt--;
				if (menu_cnt == 0) {
					p6logd("menu_cnt 0");
				}
			}
#endif
			if (SplashFlag) {
				SplashFlag--;
				if (SplashFlag == 0)
					WinDraw_HideSplash();
			}
		}
#ifndef PSP
		menu_key_down = SDLK_UNKNOWN;

		while (SDL_PollEvent(&ev)) {
			switch (ev.type) {
			case SDL_QUIT:
				goto end_loop;
			case SDL_MOUSEMOTION:
#ifdef MOUSE_GRAB
					Mouse_Event(0, .1f * Config.MouseSpeed * ev.motion.xrel * TextDotX / WindowX,
								.1f * Config.MouseSpeed * ev.motion.yrel * TextDotY / WindowY);
#endif
					break;
                case SDL_MOUSEBUTTONDOWN:
                    if (ev.button.button == SDL_BUTTON_LEFT) Mouse_Event(1, 1, 0);
                    if (ev.button.button == SDL_BUTTON_RIGHT) Mouse_Event(2, 1, 0);
                    break;
                case SDL_MOUSEBUTTONUP:
                    if (ev.button.button == SDL_BUTTON_LEFT) Mouse_Event(1, 0, 0);
                    if (ev.button.button == SDL_BUTTON_RIGHT) Mouse_Event(2, 0, 0);
                    break;
#if defined(ANDROID) || TARGET_OS_IPHONE
			case SDL_APP_WILLENTERBACKGROUND:
				DSound_Stop();
				break;
			case SDL_APP_WILLENTERFOREGROUND:
				DSound_Play();
				break;
			case SDL_FINGERDOWN:
				//p6logd("FINGERDOWN: tid: %lld,,, x:%f y:%f", ev.tfinger.touchId, ev.tfinger.x, ev.tfinger.y);
				if (touchId == -1) {
					touchId = ev.tfinger.touchId;
				}
				break;
			case SDL_FINGERMOTION:
				float kx, ky, dx, dy;
				p6logd("FM: x:%f y:%f dx:%f dy:%f\n", ev.tfinger.x, ev.tfinger.y, ev.tfinger.dx, ev.tfinger.dy);
				if (vk_cnt == 0) {
					kx = ev.tfinger.x * 800;
					ky = ev.tfinger.y * 600;
					dx = ev.tfinger.dx * 800;
					dy = ev.tfinger.dy * 600;
					if (kbd_x < kx && kbd_x + kbd_w > kx &&
					    kbd_y < ky && kbd_y + kbd_h > ky) {
						kbd_x += dx;
						kbd_y += dy;
						if (kbd_x < 0) kbd_x = 0;
						if (kbd_y < 0) kbd_y = 0;
						if (kbd_x > 700) {
							vk_cnt = -1;
						}
						if (kbd_y > 550) kbd_y = 550;
					}
				} else if (Config.JoyOrMouse) { // Mouse mode is off when the keyboard is active
					Mouse_Event(0, ev.tfinger.dx * 50 * Config.MouseSpeed, ev.tfinger.dy * 50 * Config.MouseSpeed);
				}
				break;
#endif
			case SDL_KEYDOWN:
#if defined(ANDROID) || TARGET_OS_IPHONE
				static DWORD bef = 0;
				DWORD now;
				switch (ev.key.keysym.sym) {
				case SDLK_AC_BACK:
					now = timeGetTime();
					if (now - bef < 1000) {
						goto end_loop;
					}
					bef = now;
					break;
				case SDLK_MENU:
					if (menu_mode == menu_out) {
						menu_mode = menu_enter;
						DSound_Stop();
					} else {
						DSound_Play();
						menu_mode = menu_out;
					}
					break;
				}
#endif
//				printf("keydown: 0x%x\n", ev.key.keysym.sym);
//				printf("font %d %d\n", FONT[100], FONT[101]);
				if (ev.key.keysym.sym == SDLK_F12) {
					if (menu_mode == menu_out) {
						menu_mode = menu_enter;
						DSound_Stop();
						GRAB(SDL_FALSE);
					} else {
						DSound_Play();
						menu_mode = menu_out;
						GRAB(SDL_TRUE);
					}
				}
				if (menu_mode != menu_out) {
					menu_key_down = ev.key.keysym.sym;
					if (menu_state == ms_key && ev.key.keysym.sym == SDLK_ESCAPE) {
						DSound_Play();
						menu_mode = menu_out;
						GRAB(SDL_TRUE);
					}
				} else {
					Keyboard_KeyDown(ev.key.keysym.sym);
				}
				break;
			case SDL_KEYUP:
//				printf("keyup: 0x%x\n", ev.key.keysym.sym);
				Keyboard_KeyUp(ev.key.keysym.sym);
				break;
			}
		}
#ifndef MOUSE_GRAB
		if (SDL_GetWindowFlags(sdl_window) & SDL_WINDOW_INPUT_FOCUS) {
			int mouseX, mouseY, winX, winY;
			SDL_GetGlobalMouseState(&mouseX, &mouseY);
			SDL_GetWindowPosition(sdl_window, &winX, &winY);
			mouseTarget = ((mouseX - winX) * (int)TextDotX / WindowX + TextScrollX & 0xffff) |
						  ((mouseY - winY) * (int)TextDotY / WindowY + TextScrollY) << 16;
		}
#endif
#endif //PSP

#ifdef PSP
		if (Joystick_get_downstate_psp(PSP_CTRL_START)) {
			if (menu_mode == menu_out) { 
				menu_mode = menu_enter;
				DSound_Stop();
			} else {
				DSound_Play();
				menu_mode = menu_out;
			}
		}

		if (menu_mode == menu_out
		    && Joystick_get_downstate_psp(PSP_CTRL_SELECT)) {
			Keyboard_ToggleSkbd();
			// 2度読み除け
			Joystick_reset_downstate_psp(PSP_CTRL_SELECT);
		}

		if (menu_mode == menu_out && Keyboard_IsSwKeyboard()) {
			Joystick_mv_anapad_psp();
			Joystatic_reset_anapad_psp();
			Keyboard_skbd();
		}
#endif

#if defined(ANDROID) || TARGET_OS_IPHONE

		state = Joystick_get_vbtn_state(7);

		if (menu_mode == menu_in) {
			if (state == VBTN_OFF && menu_cnt == 0) {
				menu_cnt = -2;
			}
			if (state == VBTN_ON && menu_cnt == -2) {
				DSound_Play();
				menu_mode = menu_out;
			}
		} else if (menu_mode == menu_out) {
			if (state == VBTN_OFF && menu_cnt == -2) {
				menu_cnt = -1;
			}
			if (menu_cnt == -1 && state == VBTN_ON) {
				p6logd("menu_cnt start");
				menu_cnt = 20;
			} else if (menu_cnt > 0 && state == VBTN_OFF) {
				menu_cnt = -1;
			}
			if (menu_cnt == 0) {
				p6logd("menu mode on");
				menu_mode = menu_enter;
				DSound_Stop();
			}
		}

#endif
		if (menu_mode != menu_out) {
			int ret; 

#ifdef PSP
			Joystick_Update(TRUE);
#else
			Joystick_Update(TRUE, menu_key_down);
#endif
			ret = WinUI_Menu(menu_mode == menu_enter);
			menu_mode = menu_in;
			if (ret == WUM_MENU_END) {
				DSound_Play();
				menu_mode = menu_out;
			} else if (ret == WUM_EMU_QUIT) {
				goto end_loop;
			}
		}
#ifdef PSP
		if (exit_flag) {
			goto end_loop;
		}
#endif

#if defined(ANDROID) || TARGET_OS_IPHONE

		if (menu_mode == menu_out) {
			state = Joystick_get_vbtn_state(6);
			if (vk_cnt == -1 && state == VBTN_ON) {
				p6logd("vk_cnt start");
				vk_cnt = 20;
			} else if (vk_cnt > 0 && state == VBTN_OFF) {
				vk_cnt = -1;
			}
			if (kbd_x > 700 && vk_cnt == 0) {
				kbd_x = 0, kbd_y = 0;
				p6logd("do_kbd");
			}
			if (kbd_x < 700) {
#ifdef USE_OGLES11
				Keyboard_skbd();
#endif
			}
		}
#endif

	}
end_loop:
	cpu_writemem24_nolog(0xe8e00d, 0x31);	// SRAM書き込み許可
	Memory_WriteD(0xed0040, Memory_ReadD(0xed0040)+1); // 積算稼働時間(min.)
	Memory_WriteD(0xed0044, Memory_ReadD(0xed0044)+1); // 積算起動回数

	OPM_Cleanup();
#ifndef	NO_MERCURY
	Mcry_Cleanup();
#endif

	Joystick_Cleanup();
	SRAM_Cleanup();
	FDD_Cleanup();
	//CDROM_Cleanup();
	MIDI_Cleanup();
	DSound_Cleanup();
	WinX68k_Cleanup();
	WinDraw_Cleanup();
	WinDraw_CleanupScreen();

	SaveConfig();

#if defined(PSP)
	puts("before end");
	sceKernelExitGame();
#elif defined(ANDROID) || TARGET_OS_IPHONE
	exit(0);
#endif
	return 0;
}

