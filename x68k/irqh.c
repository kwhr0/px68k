// ---------------------------------------------------------------------------------------
//  IRQH.C - IRQ Handler (架空のデバイスにょ)
// ---------------------------------------------------------------------------------------

#include "test.h"
#include "common.h"
#include "irqh.h"
#ifdef CURMPU
#include "../m68000/c68k/c68k.h"
#endif

	BYTE	IRQH_IRQ[8], IRQH_IRQ_copy[8];
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
#ifdef CURMPU
			C68k_Set_IRQ(&C68K, i);
#endif
#ifdef NEWMPU
			newirq(i);
#endif
		return;
	    }
	}
}

int my_irqh_callback(int level)
{
    int i, vect;
	INT_CALLBACK func = IRQH_CallBack[level&7];
#if defined(CURMPU) && defined(NEWMPU)
	static int vect_copy;
	extern int getCompare(void);
	if (!getCompare()) {
		vect_copy = vect = (func)(level&7);
		for (i = 0; i < 8; i++) IRQH_IRQ_copy[i] = IRQH_IRQ[i];
	}
	else vect = vect_copy;
	if (!getCompare()) {
		for (i=7; i>0; i--)
			if (IRQH_IRQ[i]) {
				C68k_Set_IRQ(&C68K, i);
				break;
			}
	}
	else {
		for (i=7; i>0; i--)
			if (IRQH_IRQ_copy[i]) {
				newirq(i);
				break;
			}
	}
#else
	vect = (func)(level&7);
	for (i=7; i>0; i--)
		if (IRQH_IRQ[i]) {
#ifdef CURMPU
			C68k_Set_IRQ(&C68K, i);
#endif
#ifdef NEWMPU
			newirq(i);
#endif
			break;
		}
#endif
    return vect;
}
