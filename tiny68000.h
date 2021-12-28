// Tiny68000
// Copyright 2021 © Yasuo Kuwahara
// MIT License

extern "C" {
#include "memory.h"
}
#include <stdint.h>

#define M68000_TRACE		0

#if M68000_TRACE
#define M68000_TRACE_LOG(adr, data, type) \
	if (tracep->index < ACSMAX) tracep->acs[tracep->index++] = { adr, data, type }
#else
#define M68000_TRACE_LOG(adr, data, type)
#endif

class M68000 {
	using s8 = int8_t;
	using u8 = uint8_t;
	using s16 = int16_t;
	using u16 = uint16_t;
	using s32 = int32_t;
	using u32 = uint32_t;
	using s64 = int64_t;
	using u64 = uint64_t;
	using IntrVecFunc = int (*)(int);
	static constexpr int FBUFMAX = 128;
public:
	M68000();
	void Reset();
	void SetMemoryPtr(u8 *p) { m = p; }
	void SetIORange32(u32 start, u32 end) { startIO = start; endIO = end; }
	int Execute(int n);
	u32 GetPC() const { return pc; }
	void IRQ(int level) { intreq = level & 7; }
	void SetIntrVecFunc(IntrVecFunc func) { intrVecFunc = func; }
private:
	void stD(u32 n, u32 data) {
		d[n] = data;
		M68000_TRACE_LOG(n, d[n], acsStoreD);
	}
	void stD(u32 n, u32 data, int size) {
		switch (size) {
			case 0: d[n] = (d[n] & 0xffffff00) | (data & 0xff); break;
			case 1: d[n] = (d[n] & 0xffff0000) | (data & 0xffff); break;
			default: d[n] = data; break;
		}
		M68000_TRACE_LOG(n, d[n], acsStoreD);
	}
	void stA(u32 n, u32 data) {
		a[n] = data;
		M68000_TRACE_LOG(n, a[n], acsStoreA);
	}
	// customized access for px68k; byte swapped within a word
	u32 ld1(u32 adr) {
		adr &= 0xffffff;
		u32 data = adr >= startIO && adr < endIO ? Memory_ReadB(adr) : m[adr ^ 1];
		M68000_TRACE_LOG(adr, data, acsLoad8);
		return data;
	}
	u32 ld2(u32 adr) {
		adr &= 0xffffff;
		u32 data = adr >= startIO && adr < endIO ? Memory_ReadW(adr) : (u16 &)m[adr];
		M68000_TRACE_LOG(adr, data, acsLoad16);
		return data;
	}
	u32 ld4(u32 adr) {
		u32 data;
		adr &= 0xffffff;
		if (adr >= startIO && adr < endIO) {
			data = Memory_ReadW(adr) << 16;
			data |= Memory_ReadW(adr + 2);
		}
		else data = (u16 &)m[adr] << 16 | (u16 &)m[adr + 2];
		M68000_TRACE_LOG(adr, data, acsLoad32);
		return data;
	}
	void st1(u32 adr, u8 data) {
		adr &= 0xffffff;
		if (adr >= startIO && adr < endIO) Memory_WriteB(adr, data);
		else m[adr ^ 1] = data;
		M68000_TRACE_LOG(adr, data, acsStore8);
	}
	void st2(u32 adr, u16 data) {
		adr &= 0xffffff;
		if (adr >= startIO && adr < endIO) Memory_WriteW(adr, data);
		else (u16 &)m[adr] = data;
		M68000_TRACE_LOG(adr, data, acsStore16);
	}
	void st4(u32 adr, u32 data) {
		adr &= 0xffffff;
		if (adr >= startIO && adr < endIO) {
			Memory_WriteW(adr, data >> 16);
			Memory_WriteW(adr + 2, data);
		}
		else {
			(u16 &)m[adr] = data >> 16;
			(u16 &)m[adr + 2] = data;
		}
		M68000_TRACE_LOG(adr, data, acsStore32);
	}
	u32 get4(u32 p) { return (u16 &)m[p] << 16 | (u16 &)m[p + 2]; }
	s16 fetch2() { pc += 2; return (u16 &)m[pc - 2]; }
	u32 fetch4() { pc += 4; return (u16 &)m[pc - 4] << 16 | (u16 &)m[pc - 2]; }
	// customized access -- end
	u32 ldS(u32 adr, int size) {
		switch (size) {
			case 0: return ld1(adr);
			case 1: return ld2(adr);
			default: return ld4(adr);
		}
	}
	void stS(u32 adr, u32 data, int size) {
		switch (size) {
			case 0: st1(adr, data); break;
			case 1: st2(adr, data); break;
			default: st4(adr, data); break;
		}
	}
	void push2(u16 d) { st2(a[7] -= 2, d); }
	void push4(u32 d) { st4(a[7] -= 4, d); }
	u32 pop2() { a[7] += 2; return ld2(a[7] - 2); }
	u32 pop4() { a[7] += 4; return ld4(a[7] - 4); }
	u32 getImm(int size) { return size < 2 ? fetch2() : fetch4(); }
	template<int RW, typename F> u32 ea(int modereg, int size, F func);
	template<int OP> void movem(u32 op);
	template<int A, typename F, typename S, typename U> void op1(u32 op, F f, S s, U u);
	template<typename F1, typename F2> void logccr(u32 op, F1 func1, F2 func2);
	template<int W, typename F> void bitop(u32 op, F func);
	template<int S> void bcd(u32 op);
	template<int B> void cond(u32 op);
	void sftrot(u32 op);
	void movep(u32 op);
	void SetSR(u16 data, bool perm = false);
	void Trap(u32 vector);
	void undef();
	int ResolvC();
	int ResolvV();
	int ResolvZ();
	int ResolvN();
	int ResolvX();
	int ResolvFlags();
	void SetupFlags(int x);
	u32 fset(u32 dm, u32 s = 0, u32 d = 0, u32 r = 0, u8 size = 0) {
		fp->dm = dm; fp->s = s; fp->d = d; fp->r = r; fp->size = size;
		if (++fp >= fbuf + FBUFMAX) ResolvFlags();
		return r;
	}
	struct FlagDecision {
		u32 dm;
		u32 s, d, r;
		u8 size;
	};
	FlagDecision fbuf[FBUFMAX];
	FlagDecision *fp;
	u8 *m;
	u32 a[8], d[8];
	u32 pc, ssp, usp;
	u16 sr;
	int intreq;
	u32 startIO, endIO;
	bool stopf;
	IntrVecFunc intrVecFunc;
#if M68000_TRACE
	static constexpr int TRACEMAX = 10000;
	static constexpr int ACSMAX = 32;
	enum {
		acsStoreD = 1, acsStoreA,
		acsStore8, acsStore16, acsStore32, acsLoad8, acsLoad16, acsLoad32
	};
	struct Acs {
		u32 adr, data;
		u8 type;
	};
	struct TraceBuffer {
		u32 pc;
		Acs acs[ACSMAX];
		u8 index, ccr;
	};
	TraceBuffer tracebuf[TRACEMAX];
	TraceBuffer *tracep;
	void StopTrace();
#endif
};
