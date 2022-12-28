// Tiny68000
// Copyright 2021,2022 © Yasuo Kuwahara
// MIT License

#include "tiny68000.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>

enum {
	LC, LV, LZ, LN, LX, LI = 8, LM = 12, LS
};

enum {
	MC = 1 << LC, MV = 1 << LV, MZ = 1 << LZ, MN = 1 << LN, MX = 1 << LX,
	MI = 7 << LI, MM = 1 << LM, MS = 1 << LS
};

enum {
	FB = 1, F0, FS, FSX, FADD, FSUB, FSUBX, FMUL, FBCD, FBF,
	FSFTROT, FSL = FSFTROT, FSR, FXL, FXR
};

#define F(flag, type)	flag##type = F##type << (L##flag << 2)
enum {
	F(C, B), F(C, 0), F(C, ADD), F(C, SUB), F(C, SL), F(C, SR), F(C, XL), F(C, XR), F(C, BCD),
	F(V, B), F(V, 0), F(V, S), F(V, ADD), F(V, SUB), F(V, MUL), F(V, SL),
	F(Z, B), F(Z, 0), F(Z, S), F(Z, SX), F(Z, BCD), F(Z, MUL), F(Z, BF),
	F(N, B), F(N, 0), F(N, S), F(N, BF),
	F(X, B), F(X, ADD), F(X, SUB), F(X, SFTROT), F(X, SL), F(X, SR), F(X, BCD)
};

// M68000PRM.pdf page 3-18
#define fadd(s, d, r, sz)	fset(XADD | NS | ZS | VADD | CADD, s, d, r, sz)
#define faddx(s, d, r, sz)	fset(XADD | NS | ZSX | VADD | CADD, s, d, r, sz)
#define fsub(s, d, r, sz)	fset(XSUB | NS | ZS | VSUB | CSUB, s, d, r, sz)
#define fsubx(s, d, r, sz)	fset(XSUB | NS | ZSX | VSUB | CSUB, s, d, r, sz)
#define fcmp(s, d, r, sz)	fset(NS | ZS | VSUB | CSUB, s, d, r, sz)
#define fmul(s, d, r, type)	fset(NS | ZMUL | VMUL | C0, s, d, r, type)
#define flogic(r, sz)		fset(NS | ZS | V0 | C0, 0, 0, r, sz)
#define fbtst(r, sz)		fset(ZS, 0, 0, r, sz)
#define fbf(r, width)		fset(NBF | ZBF | V0 | C0, 0, 0, r, width)
#define fbcd(x, r)			fset(XBCD | N0 | ZSX | V0 | CBCD, x, 0, r)
#define fchk(r)				fset(NS | Z0 | V0 | C0, 0, 0, r)

#define X 			(ResolvX() != 0)
#define SP			a[7]
#define PRIV		if (!(sr & MS)) Trap(8)

enum {
	CR_USP = 8, CR_VBR, CR_MSP = 11, CR_ISP
};

static void unimp(const char *s) {
	fprintf(stderr, "unimplemented: %s\n", s);
	exit(1);
}

static void error() {
	fprintf(stderr, "internal error\n");
	exit(1);
}

M68000::M68000() : m(nullptr), intrVecFunc(nullptr), startIO(0), endIO(0) {
#if M68000_TRACE
	memset(tracebuf, 0, sizeof(tracebuf));
	tracep = tracebuf;
#endif
}

void M68000::Reset() {
	memset(d, 0, sizeof(d));
	memset(a, 0, sizeof(a));
	memset(cr, 0, sizeof(cr));
	intreq = 0;
	stopf = false;
	SetupFlags(0);
	sr = MS | MI;
	a[7] = get4(0);
	pc = get4(4);
}

// RW: 0...address only 1...read 2...write 3...modify
// modereg: 6bit
// size: 0...byte 1...word 2...long
template<int RW, typename F> M68000::u32 M68000::ea(int modereg, int size, F func) {
	auto exmode = [&](u32 base) {
		u16 ext = fetch2();
		auto idx = [&]{
			u32 t = (ext & 0x8000 ? a : d)[ext >> 12 & 7];
			return (ext & 0x800 ? t : (s16)t) << (ext >> 9 & 3);
		};
		auto disp = [&](int sw)->u32 {
			switch (sw & 3) {
				case 2: return fetch2();
				case 3: return fetch4();
			}
			return 0;
		};
		u32 t;
		if (ext & 0x100) { // full extension (68020+)
			t = ext & 0x80 ? 0 : base; // BS
			if (ext & 0x40) // IS=1
				t = ext & 7 ? get4(t + disp(ext >> 4)) + disp(ext) : t + disp(ext >> 4) + disp(ext);
			else { // IS=0
				t += disp(ext >> 4);
				t = (ext & 7 ? ext & 4 ? get4(t) + idx() : get4(t + idx()) : t + idx()) + disp(ext);
			}
		}
		else t = base + idx() + s8(ext & 0xff); // brief extension
		return t;
	};
	u32 adr = 0, data = 0, mode = modereg >> 3 & 7, reg = modereg & 7;
	switch (mode) {
		case 4: if constexpr (RW != 0) { adr = a[reg] -= reg == 7 && !size ? 2 : 1 << size; break; } // -(An)
			// no break
		case 2: adr = a[reg]; break; // (An)
		case 3: adr = a[reg];
			if constexpr (RW != 0) a[reg] += reg == 7 && !size ? 2 : 1 << size; // (An)+
			break;
		case 5: adr = a[reg] + fetch2(); break; // (d16,An)
		case 6: adr = exmode(a[reg]); break;
		case 7:
			switch (reg) {
				case 0: adr = fetch2(); break; // (abs).W
				case 1: adr = fetch4(); break; // (abs).L
				case 2: adr = pc; adr += fetch2(); break; // (d16,PC)
				case 3: adr = exmode(pc); break; // (d8,PC,Xn)
			}
			break;
	}
	if constexpr (RW & 1)
		switch (mode) {
			case 0: data = d[reg]; break; // Dn
			case 1: data = a[reg]; break; // An
			case 7: if (reg == 4) { data = getImm(size); break; } // #imm
				// no break
			default: data = ldS(adr, size); break;
		}
	if constexpr (RW == 1) func(data);
	if constexpr (RW == 2) data = func();
	if constexpr (RW == 3) data = func(data);
	if constexpr ((RW & 2) == 2)
		switch (mode) {
			case 0: stD(reg, data, size); break; // dst Dn
			case 1: stA(reg, data); break; // dst An
			default: stS(adr, data, size); break;
		}
	return adr;
}

#define SIZE6		(op >> 6 & 3)
#define REG0		(op & 7)
#define REG9		(op >> 9 & 7)

template<int MR> void M68000::movem(u32 op) {
	u32 size = 1 + (op >> 6 & 1), ofs = size << 1, list = fetch2(), adr = ea<0>(op, size, []{});
	auto loop = [&](u32 mask, auto func) {
		for (int i = 0; i < 8; i++)
			if (list & mask << i) func(i);
	};
	if constexpr (MR) { // mem to reg
		if (size == 1) {
			loop(0x001, [&](int n) { stD(n, (s16)ld2(adr)); adr += ofs; });
			loop(0x100, [&](int n) { stA(n, (s16)ld2(adr)); adr += ofs; });
		}
		else {
			loop(0x001, [&](int n) { stD(n, ld4(adr)); adr += ofs; });
			loop(0x100, [&](int n) { stA(n, ld4(adr)); adr += ofs; });
		}
	}
	else { // reg to mem
		if ((op & 0x38) == 0x20) {
			loop(0x001, [&](int n) {
				if constexpr (MPU_TYPE >= 68020) { a[REG0] = adr -= ofs; stS(adr, a[7 - n], size); }
				else stS(adr -= ofs, a[7 - n], size);
			});
			loop(0x100, [&](int n) { stS(adr -= ofs, d[7 - n], size); });
		}
		else {
			loop(0x001, [&](int n) { stS(adr, d[n], size); adr += ofs; });
			loop(0x100, [&](int n) { stS(adr, a[n], size); adr += ofs; });
		}
	}
	if ((op & 0x38) == 0x18 || (op & 0x38) == 0x20) a[REG0] = adr; // write back if (An)+ or -(An)
}

template<int A, typename F, typename S, typename U> void M68000::op1(u32 op, F f, S s, U u) {
	switch (op >> 6 & 7) {
		default:
			ea<1>(op, SIZE6, [&](u32 v) { stD(REG9, f(v, d[REG9], SIZE6), SIZE6); }); // add/sub/and/or <ea>,Dn
			break;
		case 3:
			if constexpr(A) ea<1>(op, 1, [&](s16 v) { stA(REG9, f(v, a[REG9], 2)); }); // adda.w/suba.w <ea>,An
			else ea<1>(op, 1, [&](u16 v) { stD(REG9, u(v, d[REG9])); }); // mulu.w/divu.w
			break;
		case 4: case 5: case 6:
			ea<3>(op, SIZE6, [&](u32 v) { return f(d[REG9], v, SIZE6); }); // add/sub/and/or Dn,<ea>
			break;
		case 7:
			if constexpr (A) ea<1>(op, 2, [&](u32 v) { stA(REG9, f(v, a[REG9], 2)); }); // adda.l/suba.l <ea>,An
			else ea<1>(op, 1, [&](s16 v) { stD(REG9, s(v, d[REG9])); }); // muls.w/divs.w
			break;
	}
}

template<typename F1, typename F2> void M68000::logccr(u32 op, F1 funcl, F2 funcc) {
	switch (op & 0xff) {
		case 0x3c: // andi/ori/eori to CCR
			SetupFlags(sr = (sr & 0xff00) | (funcc((sr & 0xe0) | ResolvFlags(), fetch2()) & 0xff));
			break;
		case 0x7c: // andi/ori/eori to SR
			SetSR(funcc((sr & 0xffe0) | ResolvFlags(), fetch2()));
			break;
		default: // andi/ori/eori #imm,<ea>
			u32 t = getImm(SIZE6);
			ea<3>(op, SIZE6, [&](u32 v) { return funcl(t, v, SIZE6); });
			break;
	}
}

template<int D> void M68000::muldiv_l(u32 op) {
	u32 t = fetch2(), f64 = t & 0x400;
	u32 &dq = d[t >> 12 & 7], &dr = d[t & 7];
	if constexpr (D) {
		if (t & 0x800) // divs.l
			ea<1>(op, 2, [&](s32 s) {
				if (s) {
					s64 q = dq;
					if (f64) q |= (s64)dr << 32;
					dr = q % s;
					q /= s;
					if (q == (s32)q) dq = flogic((s32)q, 2);
					else fset(VS);
				}
				else { fset(C0); Trap(5); }
			});
		else // divu.l
			ea<1>(op, 2, [&](u32 s) {
				if (s) {
					u64 q = dq;
					if (f64) q |= (u64)dr << 32;
					dr = q % s;
					q /= s;
					if (q == (u32)q) dq = flogic((u32)q, 2);
					else fset(VS);
				}
				else { fset(C0); Trap(5); }
			});
	}
	else {
		if (t & 0x800) // muls.l
			ea<1>(op, 2, [&](s32 v) {
				s64 ts = (s32)dq * (s64)v;
				fmul(dq, v, f64 ? ts >> 32 : (s32)ts, f64 ? 1 : 0);
				dq = (s32)ts;
				if (f64) dr = ts >> 32;
			});
		else { // mulu.l
			ea<1>(op, 2, [&](u32 v) {
				u64 tu = dq * (u64)v;
				fmul(dq, v, f64 ? tu >> 32 : (u32)tu, f64 ? 2 : 0);
				dq = (u32)tu;
				if (f64) dr = tu >> 32;
			});
		}
	}
}

template<int M, typename F> void M68000::bitop(u32 op, F func) {
	u32 m;
	if constexpr ((M & 2) != 0) m = d[REG9];
	else m = fetch2();
	m = 1 << (m & (op & 0x38 ? 7 : 0x1f));
	int size = op & 0x38 ? 0 : 2;
	if constexpr (M & 1) { ea<3>(op, size, [&](u32 v) { fbtst(v & m, size); return func(v, m); }); }
	else ea<1>(op, size, [&](u32 v) { fbtst(v & m, size); });
}

void M68000::bitfield(u32 op) { // M68000PRM.pdf page 1-29
	u32 i, adr = 0, t = fetch2(), ofs = t & 0x800 ? d[t >> 6 & 7] : t >> 6 & 0x1f;
	u32 width = ((t & 0x20 ? d[t & 7] : t) - 1 & 0x1f) + 1, bofs = 0x40 - (ofs & 0x1f) - width;
	u64 data;
	if (op & 0x38) {
		adr = ea<0>(op, 0, []{}) + (ofs >> 3);
		data = (u64)ld4(adr) << 32 | ld4(adr + 4);
	}
	else data = (u64)d[REG0] << 32;
	auto ext = [&]{ return fbf(u32(data >> bofs & (1LL << width) - 1), width); };
	auto mask = [&]{ return ((1LL << width) - 1) << bofs; };
	switch (op & 0x700) {
		default: ext(); return; // bftst
		case 0x100: stD(t >> 12 & 7, ext()); return; // bfextu
		case 0x200: ext(); data ^= mask(); break; // bfchg
		case 0x300: stD(t >> 12 & 7, s32(ext() << (32 - width)) >> (32 - width)); return; // bfexts
		case 0x400: ext(); data &= mask(); break; // bfclr
		case 0x500: // bfffo
			i = 0;
			for (u32 d1 = ext(), m1 =  1 << (width - 1); i < width && !(d1 & m1); i++, m1 >>= 1)
				;
			d[t >> 12 & 7] = ofs + i;
			break;
		case 0x600: ext(); data |= mask(); break; //bfset
		case 0x700: data = (data & ~mask()) | ((u64)fbf(d[t >> 12 & 7], width) << bofs & mask()); break; // bfins
	}
	if (op & 0x38) {
		st4(adr, data >> 32);
		st4(adr + 4, (u32)data);
	}
	else d[REG0] = data >> 32;
}

void M68000::sftrot(u32 op) {
	static constexpr u32 dm[] = {
		XSR | NS | ZS | V0 | CSR, XSR | NS | ZS | V0 | CSR, XSR | NS | ZS | V0 | CXR, NS | ZS | V0 | CSR,
		XSL | NS | ZS | VSL | CSL, XSL | NS | ZS | V0 | CSL, XSL | NS | ZS | V0 | CXL, NS | ZS | V0 | CSL
	};
	int s6 = SIZE6, sop = (op >> 6 & 4) | (op >> (s6 < 3 ? 3 : 9) & 3), bits = s6 < 3 ? 8 << s6 : 16;
	auto f = [&](auto opfunc) {
		if (s6 < 3) { // register
			if (u32 v = d[REG0], s = op & 0x20 ? d[REG9] & 0x3f : ((op >> 9) - 1 & 7) + 1; s) {
				switch (s6) {
					case 0: v = sop & 3 ? (u8)v : (s8)v; break;
					case 1: v = sop & 3 ? (u16)v : (s16)v; break;
				}
				s = sop & 2 ? (sop & 3) == 2 ? s % ((8 << s6) + 1) : s & (8 << s6) - 1 : s > bits ? bits : s;
				stD(REG0, fset(dm[sop], s, v, opfunc(v, s), s6), s6);
			}
			else fset(NS | ZS | V0 | C0, 0, 0, v, s6);
		}
		else ea<3>(op, 1, [&](u32 v) { return fset(dm[sop], 1, v, opfunc(v, 1), 1); }); // memory
	};
	switch (sop) {
		case 0: // asr
			f([&](u32 v, u32 s) { return v >> s | (v >> (bits - 1) & 1 ? -1 << (bits - s) : 0); });
			break;
		case 1: // lsr
			f([&](u32 v, u32 s) { return v >> s; });
			break;
		case 2: // roxr
			f([&](u32 v, u32 s) { u32 t = bits + 1 - s; return v >> s | (t < 32 ? v << t : 0) | X << (bits - s); });
			break;
		case 3: // ror
			f([&](u32 v, u32 s) { return v >> s | v << (bits - s); });
			break;
		case 4: // asl
			f([&](u32 v, u32 s) { return v << s; });
			break;
		case 5: // lsl
			f([&](u32 v, u32 s) { return v << s; });
			break;
		case 6: // roxl
			f([&](u32 v, u32 s) { u32 t = bits + 1 - s; return v << s | (t < 32 ? v >> t : 0) | X << (s - 1); });
			break;
		case 7: // rol
			f([&](u32 v, u32 s) { return v << s | v >> (bits - s); });
			break;
	}
}

template<int S> void M68000::bcd(u32 op) {
	auto add = [&](u8 x, u8 y) {
		s16 l = (y & 0xf) + (x & 0xf) + X, cl = l >= 0xa, u = (y & 0xf0) + (x & 0xf0) + (cl << 4), cu = u >= 0xa0;
		return fbcd(cu, u - (cu ? 0xa0 : 0) + l - (cl ? 0xa : 0));
	};
	auto sub = [&](u8 x, u8 y) {
		s16 l = (y & 0xf) - (x & 0xf) - X, cl = l < 0, u = (y & 0xf0) - (x & 0xf0) - (cl << 4), cu = u < 0;
		return fbcd(cu, u + (cu ? 0xa0 : 0) + l + (cl ? 0xa : 0));
	};
	if constexpr (S == 2) ea<3>(op, 0, [&](u8 v) { return sub(v, 0); }); // nbcd
	else { // abcd / sbcd
		u8 r, x = op & 8 ? ld1(--a[REG9]) : d[REG9], y = op & 8 ? ld1(--a[REG0]) : d[REG0];
		if constexpr (!S) r = add(x, y);
		else r = sub(x, y);
		op & 8 ? st1(a[REG9], r) : stD(REG9, r, 0);
	}
}

void M68000::movep(u32 op) {
	u32 t, adr = a[REG0] + fetch2();
	switch (op & 0xc0) {
		case 0: // movep.w (d16,Ay),Dx
			t = ld1(adr) << 8;
			stD(REG9, t | ld1(adr + 2), 1);
			break;
		case 0x40: // movep.l (d16,Ay),Dx
			t = ld1(adr) << 24;
			t |= ld1(adr + 2) << 16;
			t |= ld1(adr + 4) << 8;
			stD(REG9, t | ld1(adr + 6));
			break;
		case 0x80: // movep.w Dx,(d16,Ay)
			t = d[REG9];
			st1(adr, t >> 8);
			st1(adr + 2, t);
			break;
		default: // movep.l Dx,(d16,Ay)
			t = d[REG9];
			st1(adr, t >> 24);
			st1(adr + 2, t >> 16);
			st1(adr + 4, t >> 8);
			st1(adr + 6, t);
			break;
	}
}

template<int B> void M68000::cond(u32 op) {
	auto f = [&]()->int {
		switch (op >> 8 & 0xf) { // M68000PRM.pdf page 3-19
			default: return true;
			case 1: return false;
			case 2: return !ResolvC() && !ResolvZ();
			case 3: return ResolvC() || ResolvZ();
			case 4: return !ResolvC();
			case 5: return ResolvC();
			case 6: return !ResolvZ();
			case 7: return ResolvZ();
			case 8: return !ResolvV();
			case 9: return ResolvV();
			case 10: return !ResolvN();
			case 11: return ResolvN();
			case 12: return ~(ResolvN() >> LN ^ ResolvV() >> LV) & 1;
			case 13: return (ResolvN() >> LN ^ ResolvV() >> LV) & 1;
			case 14: return ~(ResolvZ() >> LZ | (ResolvN() >> LN ^ ResolvV() >> LV)) & 1;
			case 15: return (ResolvZ() >> LZ | (ResolvN() >> LN ^ ResolvV() >> LV)) & 1;
		}
	};
	if constexpr (B) {
		u32 t = pc;
		switch (op & 0xff) {
			case 0: t += fetch2(); break;
			case 0xff: t += fetch4(); break;
			default: t += s8(op & 0xff); break;
		}
		if ((op & 0xf00) == 0x100) { // bsr
			push4(pc);
			pc = t;
		}
		else if (f()) pc = t; // bcc
	}
	else if ((op & 0x38) != 8)
		if ((op & 0x3f) < 0x3a) ea<2>(op, 0, [&]{ return f() ? 0xff : 0; }); // scc
		else { // trapcc
			if (!(op & 4)) pc += (op & 3) + ((op & 3) == 3);
			if (f()) Trap(7);
		}
	else if (!f()) { // dbcc
		s16 s = d[REG0] - 1;
		stD(REG0, s, 1);
		pc += s != -1 ? fetch2() - 2 : 2;
	}
	else pc += 2;
}

void M68000::SetSR(u16 data, bool perm) {
	if (!perm && !(sr & MS) && data & MS) Trap(8);
	cr[sr & MS ? sr & MM ? CR_MSP : CR_ISP : CR_USP] = a[7];
	a[7] = cr[data & MS ? data & MM ? CR_MSP : CR_ISP : CR_USP];
	SetupFlags(sr = data);
}

void M68000::Trap(u32 vector) {
	if (vector >= 0x100) {
		fprintf(stderr, "ignored trap vector: %d\n", vector);
		return;
	}
	u16 sr0 = (sr & 0xffe0) | ResolvFlags();
	SetSR(sr0 | MS, true);
	if constexpr (MPU_TYPE >= 68020) {
		if (vector >= 5 && vector <= 7) {
			push4(pc);
			push2(0x2000 | (vector & 0x3ff));
		}
		else push2(vector & 0x3ff);
	}
	push4(pc);
	push2(sr0);
	pc = get4(cr[CR_VBR] + (vector << 2));
	if (!pc) {
		fprintf(stderr, "undefined trap: %d\n", vector);
		exit(1);
	}
}

int M68000::Execute(int n) {
	auto _and = [&](u32 s, u32 d, int size) { return flogic(d & s, size); };
	auto _or = [&](u32 s, u32 d, int size) { return flogic(d | s, size); };
	auto eor = [&](u32 s, u32 d, int size) { return flogic(d ^ s, size); };
	auto add = [&](u32 s, u32 d, int size) { return fadd(s, d, d + s, size); };
	auto addx = [&](u32 s, u32 d, int size) { return faddx(s, d, d + s + X, size); };
	auto sub = [&](u32 s, u32 d, int size) { return fsub(s, d, d - s, size); };
	auto subx = [&](u32 s, u32 d, int size) { return fsubx(s, d, d - s - X, size); };
	auto cmp = [&](u32 s, u32 d, int size) { fcmp(s, d, d - s, size); };
	auto muls = [&](s16 s, s16 d) { return fmul(s, d, d * s, 2); };
	auto mulu = [&](u16 s, u16 d) { return fmul(s, d, d * s, 2); };
	auto divs = [&](s16 s, s32 d)->s32 {
		if (s) {
			s32 q = d / s;
			if (q == (s16)q) return flogic(d % s << 16 | (q & 0xffff), 1);
			else fset(VS);
		}
		else { fset(C0); Trap(5); }
		return d;
	};
	auto divu = [&](u16 s, u32 d) {
		if (s) {
			u32 q = d / s;
			if (q == (u16)q) return flogic(d % s << 16 | (q & 0xffff), 1);
			else fset(VS);
		}
		else { fset(C0); Trap(5); }
		return d;
	};
	auto chk = [&](u32 op, auto x, int size) {
		if (x < 0) { fchk(0); Trap(6); }
		else ea<1>(op, size, [&](u32 v) { if (x > (decltype(x))v) { fchk(-1); Trap(6); } });
	};
	auto move = [&](u32 op, int size) {
		if ((op & 0x1c0) != 0x40) // move <ea>,<ea>
			ea<1>(op, size, [&](u32 v) { ea<2>((op >> 3 & 0x38) | REG9, size, [&]{ return flogic(v, size); }); });
		else if (size == 1) ea<1>(op, 1, [&](s16 v) { stA(REG9, v); }); // movea.w <ea>,An
		else ea<1>(op, 2, [&](u32 v) { stA(REG9, v); }); // movea.l <ea>,An;
	};
	auto arithx = [&](u32 op, auto func) {
		if (op & 8) { // addx/subx -(Ay),-(Ax)
			u32 s = ldS(a[REG0] -= 1 << SIZE6, SIZE6), v = ldS(a[REG9] -= 1 << SIZE6, SIZE6);
			stS(a[REG9], func(s, v, SIZE6), SIZE6);
		}
		else stD(REG9, func(d[REG0], d[REG9], SIZE6), SIZE6); // addx/subx Dy,Dx
	};
	int clock = 0;
	do {
#ifndef TEST68K
		if (intreq > (sr >> LI & 7) || intreq == 7) {
			int i = intreq;
			intreq = 0; // an interrupt may occur in intrVecFunc()
			Trap(intrVecFunc ? intrVecFunc(i) : 24 + i);
			sr = (sr & ~MI) | i << LI;
			stopf = false;
		}
#endif
		if (stopf) return 0;
#if M68000_TRACE
		tracep->pc = pc;
		tracep->index = 0;
#endif
		u32 t;
		switch (u16 op = fetch2(); op & 0xf000) {
			case 0:
				switch (op & 0xfc0) {
					case 0: case 0x40: case 0x80:
						logccr(op, _or, [](u16 a, u16 b) { return a | b; });
						break;
					case 0xc0: unimp("CHK2.B/CMP2.B"); break;
					case 0x200: case 0x240: case 0x280:
						logccr(op, _and, [](u16 a, u16 b) { return a & b; });
						break;
					case 0x2c0: unimp("CHK2.W/CMP2.W"); break;
					case 0x400: case 0x440: case 0x480:
						t = getImm(SIZE6);
						ea<3>(op, SIZE6, [&](u32 v) { return sub(t, v, SIZE6); }); // subi #imm,<ea>
						break;
					case 0x4c0: unimp("CHK2.L/CMP2.L"); break;
					case 0x600: case 0x640: case 0x680:
						t = getImm(SIZE6);
						ea<3>(op, SIZE6, [&](u32 v) { return add(t, v, SIZE6); }); // addi #imm,<ea>
						break;
					case 0x6c0: unimp("CALLM/RTM"); break;
					case 0x800: bitop<0>(op, []{}); break; // btst #imm,<ea>
					case 0x840: bitop<1>(op, [](u32 v, u32 m) { return v ^ m; }); break; // bchg #imm,<ea>
					case 0x880: bitop<1>(op, [](u32 v, u32 m) { return v & ~m; }); break; // bclr #imm,<ea>
					case 0x8c0: bitop<1>(op, [](u32 v, u32 m) { return v | m; }); break; // bset #imm,<ea>
					case 0xa00: case 0xa40: case 0xa80:
						logccr(op, eor, [](u16 a, u16 b) { return a ^ b; });
						break;
					case 0xac0: unimp("CAS.B"); break;
					case 0xc00: case 0xc40: case 0xc80:
						t = getImm(SIZE6);
						ea<1>(op, SIZE6, [&](u32 v) { cmp(t, v, SIZE6); }); // cmpi #imm,<ea>
						break;
					case 0xcc0: unimp("CAS.W/CAS2.W"); break;
					case 0xe00: case 0xe40: case 0xe80: unimp("MOVES"); break;
					case 0xec0: unimp("CAS.L/CAS2.L"); break;
					default:
						if ((op & 0x38) == 8) movep(op);
						else switch (op & 0xc0) {
							default: bitop<2>(op, []{}); break; // btst Dn,<ea>
							case 0x40: bitop<3>(op, [](u32 v, u32 m) { return v ^ m; }); break; // bchg Dn,<ea>
							case 0x80: bitop<3>(op, [](u32 v, u32 m) { return v & ~m; }); break; // bclr Dn,<ea>
							case 0xc0: bitop<3>(op, [](u32 v, u32 m) { return v | m; }); break; // bset Dn,<ea>
						}
						break;
				}
				break;
			case 0x1000: move(op, 0); break; // move.b
			case 0x2000: move(op, 2); break; // move.l / movea.l
			case 0x3000: move(op, 1); break; // move.w / movea.w
			case 0x4000:
				if ((op & 0x140) == 0x100 && (op & 0x38) != 8)
					if (op & 0x80) chk(op, (s16)d[REG9], 1); // chk.w
					else chk(op, (s32)d[REG9], 2); // chk.l
				else switch (op & 0xfc0) {
					case 0: case 0x40: case 0x80:
						ea<3>(op, SIZE6, [&](u32 v) { return fsubx(v, 0, -v - X, SIZE6); }); // negx
						break;
					case 0xc0:
						if constexpr (MPU_TYPE >= 68010) PRIV;
						ea<2>(op, 1, [&]{ return (sr & 0xffe0) | ResolvFlags(); }); // move from SR
						break;
					case 0x200: case 0x240: case 0x280: // clr
						if constexpr (MPU_TYPE >= 68010) ea<2>(op, SIZE6, [&]{ return flogic(0, 0); });
						else ea<3>(op, SIZE6, [&](u32) { return flogic(0, 0); });
						break;
					case 0x2c0:
						ea<2>(op, 1, [&]{ return (sr & 0xe0) | ResolvFlags(); }); // move from CCR
						break;
					case 0x400: case 0x440: case 0x480:
						ea<3>(op, SIZE6, [&](u32 v) { return fsub(v, 0, -v, SIZE6); }); // neg
						break;
					case 0x4c0:
						ea<1>(op, 1, [&](u32 v) { sr = (sr & 0xff00) | (v & 0xff); SetupFlags(v); }); // move to CCR
						break;
					case 0x600: case 0x640: case 0x680: // not
						ea<3>(op, SIZE6, [&](u32 v) { return flogic(~v, SIZE6); });
						break;
					case 0x6c0:
						ea<1>(op, 1, [&](u32 v) { SetSR(v); }); // move to SR
						break;
					case 0x800:
						if ((op & 0x38) == 8) { push4(a[REG0]); a[REG0] = SP; SP += fetch4(); } // link.l
						else bcd<2>(op); // nbcd
						break;
					case 0x840:
						switch (op & 0x38) {
							case 0: stD(REG0, flogic(d[REG0] << 16 | d[REG0] >> 16, 2)); break; // swap
							case 8: unimp("BKPT"); break;
							default: push4(ea<0>(op, 2, []{})); break; // pea
						}
						break;
					case 0x880:
						if (op & 0x38) movem<0>(op);
						else stD(REG0, flogic((s8)d[REG0], 0), 1); // ext.w
						break;
					case 0x8c0:
						if (op & 0x38) movem<0>(op);
						else flogic(d[REG0] = (s16)d[REG0], 2); // ext.l
						break;
					case 0xa00: case 0xa40: case 0xa80:
						ea<1>(op, SIZE6, [&](u32 v) { flogic(v, SIZE6); }); // tst
						break;
					case 0xac0:
						if ((op & 0x3f) != 0x3c) ea<3>(op, 0, [&](u32 v) { return flogic(v, 0) | 0x80; }); // tas
						else Trap(4); // illegal
						break;
					case 0xc00:
						muldiv_l<0>(op);
						break;
					case 0xc40:
						muldiv_l<1>(op);
						break;
					case 0xc80: case 0xcc0:
						movem<1>(op); // movem <ea>,list
						break;
					case 0xe00: undef(); break;
					case 0xe40:
						switch (op & 0x38) {
							case 0: case 8: Trap(32 + (op & 0xf)); break;
							case 0x10: // link.w
								push4(a[REG0]);
								a[REG0] = SP;
								SP += fetch2();
								break;
							case 0x18: // unlk
								SP = a[REG0];
								a[REG0] = pop4();
								break;
							case 0x20: // move to usp
								PRIV; cr[CR_USP] = a[REG0];
								break;
							case 0x28: // move from usp
								PRIV; a[REG0] = cr[CR_USP];
								break;
							default:
								switch (op & 0xf) {
									case 0: fprintf(stderr, "RESET instruction\n"); break;
									case 1: break; // nop
									case 2: // stop
										SetSR(fetch2());
										stopf = true;
										break;
									case 3: // rte
										t = pop2();
										pc = pop4();
										SetSR(t);
										if constexpr (MPU_TYPE >= 68020)
											if ((pop2() & 0xf000) == 0x2000) SP += 4;
										break;
									case 4: // rtd
										pc = pop4();
										SP += fetch2();
										break;
									case 5: // rts
										pc = pop4();
										break;
									case 6: // trapv
										if (ResolvV()) Trap(7);
										break;
									case 7: // rtr
										SetupFlags(pop2());
										pc = pop4();
										break;
									case 0xa: case 0xb: // movec
										if ((t = fetch2()) >= 8) t -= 0x7f8;
										if (t >= 16) break;
										if (op & 1) cr[t] = (t & 0x8000 ? d : a)[t >> 12 & 7];
										else (t & 0x8000 ? d : a)[t >> 12 & 7] = cr[t];
										break;
									default: undef(); break;
								}
								break;
						}
						break;
					case 0xe80: // jsr
						t = ea<0>(op, 0, []{});
						push4(pc);
						pc = t;
						break;
					case 0xec0: pc = ea<0>(op, 0, []{}); // jmp
						break;
					default:
						if ((op & 0xff8) == 0x9c0) flogic(d[REG0] = (s8)d[REG0], 2); // extb.l
						else stA(REG9, ea<0>(op, 2, []{})); // lea
						break;
				}
				break;
			case 0x5000:
				if (SIZE6 == 3) cond<0>(op);
				else ea<3>(op, (op & 0x38) == 8 ? 2 : SIZE6, [&](u32 v) { // addq/subq #imm,<ea>
					t = ((op >> 9) - 1 & 7) + 1;
					return (op & 0x38) == 8 ? op & 0x100 ? v - t : v + t : op & 0x100 ? sub(t, v, SIZE6) : add(t, v, SIZE6);
				});
				break;
			case 0x6000:
				cond<1>(op);
				break;
			case 0x7000: // moveq
				if (!(op & 0x100)) d[REG9] = flogic(s8(op & 0xff), 2);
				else undef();
				break;
			case 0x8000:
				if (!(op & 0x30) && (t = op >> 6 & 7) >= 4 && t <= 6)
					switch (t) {
						default: bcd<1>(op); break; // sbcd
						case 5: unimp("PACK"); break;
						case 6: unimp("UNPK"); break;
					}
				else op1<0>(op, _or, divs, divu);
				break;
			case 0x9000:
				if (op & 0x100 && (op & 0xc0) != 0xc0 && !(op & 0x30)) arithx(op, subx);
				else if ((op & 0xc0) != 0xc0) op1<1>(op, sub, []{}, []{}); // sub
				else op1<1>(op, [&](u32 s, u32 d, int) { return d - s; }, []{}, []{}); // suba
				break;
			case 0xa000:
				pc -= 2;
				Trap(10);
				break;
			case 0xb000:
				switch (op >> 6 & 7) {
					default: // cmp <ea>,Dn
						ea<1>(op, SIZE6, [&](u32 v) { cmp(v, d[REG9], SIZE6); });
						break;
					case 3: // cmpa.w <ea>,An
						ea<1>(op, 1, [&](s16 v) { cmp(v, a[REG9], 2); });
						break;
					case 4: case 5: case 6:
						if ((op & 0x38) == 8) { // cmpm (Ay)+,(Ax)+
							cmp(ldS(a[REG0], SIZE6), ldS(a[REG9], SIZE6), SIZE6);
							a[REG0] += 1 << SIZE6;
							a[REG9] += 1 << SIZE6;
						}
						else ea<3>(op, SIZE6, [&](u32 v) { return eor(d[REG9], v, SIZE6); }); // eor Dn,<ea>
						break;
					case 7: // cmpa.l <ea>,An
						ea<1>(op, 2, [&](u32 v) { cmp(v, a[REG9], 2); });
						break;
				}
				break;
			case 0xc000:
				if (!(op & 0x30) && (t = op >> 6 & 7) >= 4 && t <= 6)
					switch (t) {
						default: bcd<0>(op); break; // abcd
						case 5:
							if (op & 8) std::swap(a[REG9], a[REG0]); // exg Ax,Ay
							else std::swap(d[REG9], d[REG0]); // exg Dx,Dy
							break;
						case 6:
							if (op & 8) std::swap(d[REG9], a[REG0]); // exg Dx,Ay
							else undef();
							break;
					}
				else op1<0>(op, _and, muls, mulu);
				break;
			case 0xd000:
				if (op & 0x100 && (op & 0xc0) != 0xc0 && !(op & 0x30)) arithx(op, addx);
				else if ((op & 0xc0) != 0xc0) op1<1>(op, add, []{}, []{}); // add
				else op1<1>(op, [&](u32 s, u32 d, int) { return d + s; }, []{}, []{}); // adda
				break;
			case 0xe000:
				(op & 0x8c0) == 0x8c0 ? bitfield(op) : sftrot(op);
				break;
			case 0xf000:
				pc -= 2;
				Trap(11); // CINV,cp*,CPUSH,FPinst,MOVE16
				break;
		}
#ifdef TEST68K
		if (pc == 0xf000) {
			printf("TEST PASSED.\n");
			exit(0);
		}
#endif
#if M68000_TRACE
		tracep->ccr = ResolvFlags();
#if M68000_TRACE > 1
		if (++tracep >= tracebuf + TRACEMAX - 1) StopTrace();
#else
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
#endif
#endif
		clock += 10; // CPI
	} while (!stopf && clock < n);
	return stopf ? 0 : clock - n;
}

#define MSB_N	((8 << p->size) - 1)
#define IS0		!(p->r & u32((1LL << (8 << p->size)) - 1))

int M68000::ResolvC() {
	u32 sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && (!(sw = p->dm & 0xf) || ((sw == CXL || sw == CXR) && !p->s)); p--)
		;
	if (p < fbuf) error();
	switch (sw) {
		case F0:
			break;
		case FB:
			return p->r & MC;
		case FADD:
			return (((p->s & p->d) | (~p->r & p->d) | (p->s & ~p->r)) >> MSB_N & 1) << LC;
		case FSUB:
			return (((p->s & ~p->d) | (p->r & ~p->d) | (p->s & p->r)) >> MSB_N & 1) << LC;
		case FSL: case FXL:
			return (p->d >> (p->s ? (8 << p->size) - p->s : 0) & 1) << LC;
		case FSR: case FXR:
			return (p->d >> (p->s ? p->s - 1 : (8 << p->size) - 1) & 1) << LC;
		case FBCD:
			return (p->s != 0) << LC;
		default:
			error();
			break;
	}
	return 0;
}

int M68000::ResolvV() {
	u32 sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf0); p--)
		;
	if (p < fbuf) error();
	s64 ts;
	u64 tu;
	switch (sw >> 4) {
		case F0:
			break;
		case FB:
			return p->r & MV;
		case FS:
			return MV; // for div
		case FADD:
			return (((p->s & p->d & ~p->r) | (~p->s & ~p->d & p->r)) >> MSB_N & 1) << LV;
		case FSUB:
			return (((~p->s & p->d & ~p->r) | (p->s & ~p->d & p->r)) >> MSB_N & 1) << LV;
		case FMUL:
			switch (p->size) {
				case 1: ts = (s64)(s32)p->s * (s32)p->d; return ts != (s32)ts;
				case 2: tu = (u64)p->s * p->d; return tu != (u32)tu;
			}
			return 0;
		case FSL:
		{ // ex) count=4,bits=8  V=0 if Mmmmmxxx all of m equal M
			u32 m = ((1 << p->s) - 1) << ((8 << p->size) - p->s - 1);
			return (((p->d >> MSB_N & 1 ? ~p->d : p->d) & m) != 0) << LV;
		}
		default:
			error();
			break;
	}
	return 0;
}

int M68000::ResolvZ() {
	u32 sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && (!(sw = p->dm & 0xf00) || (sw == ZSX && IS0)); p--)
		;
	if (p < fbuf) error();
	switch (sw >> 8) {
		case F0: case FSX:
			break;
		case FB:
			return p->r & MZ;
		case FS:
			return IS0 << LZ;
		case FMUL:
			return (!p->s || !p->d) << LZ;
		case FBF:
			return !(p->r & (1LL << p->size) - 1) << LZ;
		default:
			error();
			break;
	}
	return 0;
}

int M68000::ResolvN() {
	u32 sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf000); p--)
		;
	if (p < fbuf) error();
	switch (sw >> 12) {
		case F0:
			break;
		case FB:
			return p->r & MN;
		case FS:
			return (p->r >> ((8 << p->size) - 1) & 1) << LN;
		case FBF:
			return (p->r >> (p->size - 1) & 1) << LN;
		default:
			error();
			break;
	}
	return 0;
}

int M68000::ResolvX() {
	u32 sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && (!(sw = p->dm & 0xf0000) || (sw >= XSFTROT && !p->s)); p--)
		;
	if (p < fbuf) error();
	switch (sw >> 16) {
		case F0:
			break;
		case FB:
			return p->r & MX;
		case FADD:
			return (((p->s & p->d) | (~p->r & p->d) | (p->s & ~p->r)) >> MSB_N & 1) << LX;
		case FSUB:
			return (((p->s & ~p->d) | (p->r & ~p->d) | (p->s & p->r)) >> MSB_N & 1) << LX;
		case FSL:
			return (p->d >> ((8 << p->size) - p->s) & 1) << LX;
		case FSR:
			return (p->d >> (p->s - 1) & 1) << LX;
		case FBCD:
			return (p->s != 0) << LX;
		default:
			error();
			break;
	}
	return 0;
}

void M68000::SetupFlags(int x) {
	fp = fbuf;
	fp->dm = XB | NB | ZB | VB | CB;
	fp++->r = x;
}

int M68000::ResolvFlags() {
	int r = ResolvX() | ResolvN() | ResolvZ() | ResolvV() | ResolvC();
	SetupFlags(r);
	return r;
}

#if M68000_TRACE
#include <string>
void M68000::StopTrace() {
	TraceBuffer *endp = tracep;
	int i = 0;
	FILE *fo;
	if (!(fo = fopen((std::string(getenv("HOME")) + "/Desktop/trace.txt").c_str(), "w"))) exit(1);
	do {
		if (++tracep >= tracebuf + TRACEMAX) tracep = tracebuf;
		fprintf(fo, "%4d %06x ", i++, tracep->pc);
		// px68k: byte swapped within a word
		if (tracep->pc < 0x1000000) fprintf(fo, "%04x ", m[tracep->pc] | m[tracep->pc + 1] << 8);
		else fprintf(fo, "???? ");
		fprintf(fo, "%c%c%c%c%c ",
				tracep->ccr & 0x10 ? 'X' : '-',
				tracep->ccr & 0x08 ? 'N' : '-',
				tracep->ccr & 0x04 ? 'Z' : '-',
				tracep->ccr & 0x02 ? 'V' : '-',
				tracep->ccr & 0x01 ? 'C' : '-');
		for (Acs *p = tracep->acs; p < tracep->acs + tracep->index; p++) {
			switch (p->type) {
				case acsLoad8:
					fprintf(fo, "L %06x %02x ", p->adr, p->data & 0xff);
					break;
				case acsLoad16:
					fprintf(fo, "L %06x %04x ", p->adr, p->data & 0xffff);
					break;
				case acsLoad32:
					fprintf(fo, "L %06x %08x ", p->adr, p->data);
					break;
				case acsStore8:
					fprintf(fo, "S %06x %02x ", p->adr, p->data & 0xff);
					break;
				case acsStore16:
					fprintf(fo, "S %06x %04x ", p->adr, p->data & 0xffff);
					break;
				case acsStore32:
					fprintf(fo, "S %06x %08x ", p->adr, p->data);
					break;
				case acsStoreD:
					fprintf(fo, "%08x->D%d ", p->data, p->adr);
					break;
				case acsStoreA:
					fprintf(fo, "%08x->A%d ", p->data, p->adr);
					break;
			}
		}
		fprintf(fo, "\n");
	} while (tracep != endp);
	fclose(fo);
	fprintf(stderr, "trace dumped.\n");
	exit(1);
}
#endif

void M68000::undef() {
	// px68k: byte swapped within a word
	fprintf(stderr, "undefined instruction: PC=%06x OP=%04x\n", pc - 2, m[pc - 2] | m[pc - 1] << 8);
#if M68000_TRACE
	StopTrace();
#endif
	exit(1);
}
