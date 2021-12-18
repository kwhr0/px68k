#ifndef _winx68k_irqh
#define _winx68k_irqh

#include "common.h"

typedef int (*INT_CALLBACK)(int);

void IRQH_Init(void);
DWORD FASTCALL IRQH_DefaultVector(BYTE irq);
void IRQH_IRQCallBack(BYTE irq);
void IRQH_Int(BYTE irq, void* handler);
int my_irqh_callback(int level);

#endif
