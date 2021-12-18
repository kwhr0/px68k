#ifndef _winx68k_scsi
#define _winx68k_scsi

#include "common.h"

void SCSI_Init(void);
void SCSI_Cleanup(void);

BYTE FASTCALL SCSI_Read(DWORD adr);
void FASTCALL SCSI_Write(DWORD adr, BYTE data);

#endif

