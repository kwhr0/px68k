// ---------------------------------------------------------------------------------------
//  IRQH.C - IRQ Handler (架空のデバイスにょ)
// ---------------------------------------------------------------------------------------

#include "test.h"
#include "common.h"
#include "irqh.h"
#ifndef NEWMPU
#include "../m68000/c68k/c68k.h"
#endif

	BYTE	IRQH_IRQ[8];
	void	*IRQH_CallBack[8];

// -----------------------------------------------------------------------
//   初期化
// -----------------------------------------------------------------------
void IRQH_Init(void)
{
	ZeroMemory(IRQH_IRQ, 8);
}


// -----------------------------------------------------------------------
//   デフォルトのベクタを返す（これが起こったら変だお）
// -----------------------------------------------------------------------
DWORD FASTCALL IRQH_DefaultVector(BYTE irq)
{
	IRQH_IRQCallBack(irq);
	return -1;
}


// -----------------------------------------------------------------------
//   他の割り込みのチェック
//   各デバイスのベクタを返すルーチンから呼ばれます
// -----------------------------------------------------------------------
void IRQH_IRQCallBack(BYTE irq)
{
	IRQH_IRQ[irq&7] = 0;
}


// -----------------------------------------------------------------------
//   割り込み発生
// -----------------------------------------------------------------------

void IRQH_Int(BYTE irq, void* handler)
{
	int i;
	IRQH_IRQ[irq&7] = 1;
	if (handler==NULL)
	    IRQH_CallBack[irq&7] = &IRQH_DefaultVector;
	else
	    IRQH_CallBack[irq&7] = handler;
	for (i=7; i>0; i--)
	{
	    if (IRQH_IRQ[i])
	    {
#ifdef NEWMPU
			newirq(i);
#else
			C68k_Set_IRQ(&C68K, i);
#endif
		return;
	    }
	}
}

int my_irqh_callback(int level)
{
    int i;
    INT_CALLBACK func = IRQH_CallBack[level&7];
    int vect = (func)(level&7);
//    printf("irq vect = %x line = %d\n", vect, level);

    for (i=7; i>0; i--)
    {
	if (IRQH_IRQ[i])
	{
#ifdef NEWMPU
		newirq(i);
#else
	    C68k_Set_IRQ(&C68K, i);
#endif
	    break;
	}
    }

    return vect;
}
