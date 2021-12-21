// ---------------------------------------------------------------------------------------
//  SYSPORT.C - X68k System Port
// ---------------------------------------------------------------------------------------

#include "common.h"
#include "prop.h"
#include "sysport.h"
#include "palette.h"
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>

#define OPEN			0xe8e011
#define CLOSE			0xe8e013
#define DATA			0xe8e015
#define VALID			0xe8e017
#define MOUSE			0xe8e019

BYTE	SysPort[7];
DWORD mouseTarget;
static int sock = -1, read_ret, mouse_data;

// -----------------------------------------------------------------------
//   初期化
// -----------------------------------------------------------------------
void SysPort_Init(void)
{
	int i;
	for (i=0; i<7; i++) SysPort[i]=0;
}


// -----------------------------------------------------------------------
//   らいと
// -----------------------------------------------------------------------
void FASTCALL SysPort_Write(DWORD adr, BYTE data)
{
	switch(adr)
	{
	case 0xe8e001:
		if (SysPort[1]!=(data&15))
		{
			SysPort[1] = data & 15;
			Pal_ChangeContrast(SysPort[1]);
		}
		break;
	case 0xe8e003:
		SysPort[2] = data & 0x0b;
		break;
	case 0xe8e005:
		SysPort[3] = data & 0x1f;
		break;
	case 0xe8e007:
		SysPort[4] = data & 0x0e;
		break;
	case 0xe8e00d:
		SysPort[5] = data;
		break;
	case 0xe8e00f:
		SysPort[6] = data & 15;
		break;
	case OPEN:
		sock = socket(AF_UNIX, SOCK_STREAM, 0);
		if (sock == -1) break;
		struct sockaddr_un sa = { 0 };
		sa.sun_family = AF_UNIX;
		strcpy(sa.sun_path, "/tmp/px68k");
		if (connect(sock, (struct sockaddr *)&sa, sizeof(struct sockaddr_un)) == -1) {
			close(sock);
			sock = -1;
		}
		break;
	case CLOSE:
		if (sock != -1) close(sock);
		sock = -1;
		break;
	case DATA:
		if (sock != -1) write(sock, &data, 1);
		break;
	case MOUSE:
		mouse_data = mouseTarget;
		break;
	}
}


// -----------------------------------------------------------------------
//   りーど
// -----------------------------------------------------------------------
BYTE FASTCALL SysPort_Read(DWORD adr)
{
	BYTE ret=0xff;

	switch(adr)
	{
	case 0xe8e001:
		ret = SysPort[1];
		break;
	case 0xe8e003:
		ret = SysPort[2];
		break;
	case 0xe8e005:
		ret = SysPort[3];
		break;
	case 0xe8e007:
		ret = SysPort[4];
		break;
	case 0xe8e00b:		// 10MHz:0xff、16MHz:0xfe、030(25MHz):0xdcをそれぞれ返すらしい
		switch(Config.XVIMode)
		{
		case 1:			// XVI or RedZone
		case 2:
			ret = 0xfe;
			break;
		case 3:			// 030
			ret = 0xdc;
			break;
		default:		// 10MHz
			ret = 0xff;
			break;
		}
		break;
	case 0xe8e00d:
		ret = SysPort[5];
		break;
	case 0xe8e00f:
		ret = SysPort[6];
		break;
	case OPEN:
		ret = sock != -1;
		break;
	case DATA:
		read_ret = sock != -1 ? (int)read(sock, &ret, 1) : 0;
		break;
	case VALID:
		ret = read_ret == 1;
		break;
	case MOUSE:
		ret = mouse_data;
		mouse_data >>= 8;
		break;
	}

	return ret;
}
