#ifndef _winx68k_sram
#define _winx68k_sram

#include "common.h"

void SRAM_Init(void);
void SRAM_Cleanup(void);
void SRAM_VirusCheck(void);

BYTE FASTCALL SRAM_Read(DWORD adr);
void FASTCALL SRAM_Write(DWORD adr, BYTE data);

#endif

