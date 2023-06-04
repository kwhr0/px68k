// Tiny68000
// Copyright 2021-2023 © Yasuo Kuwahara
// MIT License

#include "tiny68000.h"
#include <cstring>

#define P(x)			(cnv.pmf = &M68000::x, cnv.p)
#define PI(x, i)		(cnv.pmf = &M68000::x<i>, cnv.p)
#define PIS(x, i, s)	(cnv.pmf = &M68000::x<i, s>, cnv.p)
#define PM(i, j, s)		(cnv.pmf = &M68000::move_ea_ea<i, j, s>, cnv.p)

#define MODE(op, mask, x) {\
add(op | 0x00, mask | 0x38, PI(x, 0));\
add(op | 0x08, mask | 0x38, PI(x, 1));\
add(op | 0x10, mask | 0x38, PI(x, 2));\
add(op | 0x18, mask | 0x38, PI(x, 3));\
add(op | 0x20, mask | 0x38, PI(x, 4));\
add(op | 0x28, mask | 0x38, PI(x, 5));\
add(op | 0x30, mask | 0x38, PI(x, 6));\
add(op | 0x38, mask | 0x3f, PI(x, 8));\
add(op | 0x39, mask | 0x3f, PI(x, 9));\
add(op | 0x3a, mask | 0x3f, PI(x, 10));\
add(op | 0x3b, mask | 0x3f, PI(x, 11));\
add(op | 0x3c, mask | 0x3f, PI(x, 12));\
add(op | 0x1f, mask | 0x3f, PI(x, 13));\
add(op | 0x27, mask | 0x3f, PI(x, 14));\
}
#define MODES(op, mask, x, s) {\
add(op | 0x00, mask | 0x38, PIS(x, 0, s));\
add(op | 0x08, mask | 0x38, PIS(x, 1, s));\
add(op | 0x10, mask | 0x38, PIS(x, 2, s));\
add(op | 0x18, mask | 0x38, PIS(x, 3, s));\
add(op | 0x20, mask | 0x38, PIS(x, 4, s));\
add(op | 0x28, mask | 0x38, PIS(x, 5, s));\
add(op | 0x30, mask | 0x38, PIS(x, 6, s));\
add(op | 0x38, mask | 0x3f, PIS(x, 8, s));\
add(op | 0x39, mask | 0x3f, PIS(x, 9, s));\
add(op | 0x3a, mask | 0x3f, PIS(x, 10, s));\
add(op | 0x3b, mask | 0x3f, PIS(x, 11, s));\
add(op | 0x3c, mask | 0x3f, PIS(x, 12, s));\
add(op | 0x1f, mask | 0x3f, PIS(x, 13, s));\
add(op | 0x27, mask | 0x3f, PIS(x, 14, s));\
}
#define S3(op, mask, x) {\
add(op | 0x00, mask, PI(x, 0));\
add(op | 0x40, mask, PI(x, 1));\
add(op | 0x80, mask, PI(x, 2));\
}
#define IS3(op, mask, x) {\
MODES(op | 0x00, mask, x, 0);\
MODES(op | 0x40, mask, x, 1);\
MODES(op | 0x80, mask, x, 2);\
}
#define ME(op, mask, i, s) {\
add(op | 0x00, mask | 0x38, PM(0, i, s));\
add(op | 0x08, mask | 0x38, PM(1, i, s));\
add(op | 0x10, mask | 0x38, PM(2, i, s));\
add(op | 0x18, mask | 0x38, PM(3, i, s));\
add(op | 0x20, mask | 0x38, PM(4, i, s));\
add(op | 0x28, mask | 0x38, PM(5, i, s));\
add(op | 0x30, mask | 0x38, PM(6, i, s));\
add(op | 0x38, mask | 0x3f, PM(8, i, s));\
add(op | 0x39, mask | 0x3f, PM(9, i, s));\
add(op | 0x3a, mask | 0x3f, PM(10, i, s));\
add(op | 0x3b, mask | 0x3f, PM(11, i, s));\
add(op | 0x3c, mask | 0x3f, PM(12, i, s));\
add(op | 0x1f, mask | 0x3f, PM(13, i, s));\
add(op | 0x27, mask | 0x3f, PM(14, i, s));\
}
#define MEE(op, s) {\
ME(op | 0x0000, 0xf1c0, 0, s);\
ME(op | 0x0040, 0xf1c0, 1, s);\
ME(op | 0x0080, 0xf1c0, 2, s);\
ME(op | 0x00c0, 0xf1c0, 3, s);\
ME(op | 0x0100, 0xf1c0, 4, s);\
ME(op | 0x0140, 0xf1c0, 5, s);\
ME(op | 0x0180, 0xf1c0, 6, s);\
ME(op | 0x01c0, 0xffc0, 8, s);\
ME(op | 0x03c0, 0xffc0, 9, s);\
ME(op | 0x05c0, 0xffc0, 10, s);\
ME(op | 0x07c0, 0xffc0, 11, s);\
ME(op | 0x09c0, 0xffc0, 12, s);\
ME(op | 0x0ec0, 0xffc0, 13, s);\
ME(op | 0x0f00, 0xffc0, 14, s);\
}
#define CC(op, mask, x) {\
add(op | 0x000, mask, PI(x, 0));\
add(op | 0x100, mask, PI(x, 1));\
add(op | 0x200, mask, PI(x, 2));\
add(op | 0x300, mask, PI(x, 3));\
add(op | 0x400, mask, PI(x, 4));\
add(op | 0x500, mask, PI(x, 5));\
add(op | 0x600, mask, PI(x, 6));\
add(op | 0x700, mask, PI(x, 7));\
add(op | 0x800, mask, PI(x, 8));\
add(op | 0x900, mask, PI(x, 9));\
add(op | 0xa00, mask, PI(x, 10));\
add(op | 0xb00, mask, PI(x, 11));\
add(op | 0xc00, mask, PI(x, 12));\
add(op | 0xd00, mask, PI(x, 13));\
add(op | 0xe00, mask, PI(x, 14));\
add(op | 0xf00, mask, PI(x, 15));\
}
#define BCC(op, mask, s) {\
add(op | 0x000, mask, PIS(bcc, 0, s));\
add(op | 0x100, mask, PIS(bcc, 1, s));\
add(op | 0x200, mask, PIS(bcc, 2, s));\
add(op | 0x300, mask, PIS(bcc, 3, s));\
add(op | 0x400, mask, PIS(bcc, 4, s));\
add(op | 0x500, mask, PIS(bcc, 5, s));\
add(op | 0x600, mask, PIS(bcc, 6, s));\
add(op | 0x700, mask, PIS(bcc, 7, s));\
add(op | 0x800, mask, PIS(bcc, 8, s));\
add(op | 0x900, mask, PIS(bcc, 9, s));\
add(op | 0xa00, mask, PIS(bcc, 10, s));\
add(op | 0xb00, mask, PIS(bcc, 11, s));\
add(op | 0xc00, mask, PIS(bcc, 12, s));\
add(op | 0xd00, mask, PIS(bcc, 13, s));\
add(op | 0xe00, mask, PIS(bcc, 14, s));\
add(op | 0xf00, mask, PIS(bcc, 15, s));\
}

static struct Insn {
	using pmf_t = void (M68000::*)(uint16_t);
	using pf_t = void (*)(M68000 *, uint16_t);
	Insn() {
		for (int i = 0; i < 0x10000; i++) fn[i] = P(undef);
		IS3(0x0000, 0xffc0, ori_ea);
		add(0x003c, 0xffff, P(ori_ccr));
		add(0x007c, 0xffff, P(ori_sr));
		add(0x00c0, 0xf9c0, P(x00c0));
		MODE(0x0100, 0xf1c0, btst);
		MODE(0x0140, 0xf1c0, bchg);
		MODE(0x0180, 0xf1c0, bclr);
		MODE(0x01c0, 0xf1c0, bset);
		add(0x0108, 0xf138, P(movep)); // overwrite bitop
		IS3(0x0200, 0xffc0, andi_ea);
		add(0x023c, 0xffff, P(andi_ccr));
		add(0x027c, 0xffff, P(andi_sr));
		IS3(0x0400, 0xffc0, subi);
		IS3(0x0600, 0xffc0, addi);
		MODE(0x0800, 0xffc0, btst_i);
		MODE(0x0840, 0xffc0, bchg_i);
		MODE(0x0880, 0xffc0, bclr_i);
		add(0x08c0, 0xf9c0, P(x08c0));
		MODE(0x08c0, 0xffc0, bset_i); // overwrite x08c0
		IS3(0x0a00, 0xffc0, eori_ea);
		add(0x0a3c, 0xffff, P(eori_ccr));
		add(0x0a7c, 0xffff, P(eori_sr));
		IS3(0x0c00, 0xffc0, cmpi);
		MEE(0x1000, 0);
		MEE(0x2000, 2);
		MODE(0x2040, 0xf1c0, movea_l);
		MEE(0x3000, 1);
		MODE(0x3040, 0xf1c0, movea_w);
		IS3(0x4000, 0xffc0, negx);
		MODE(0x40c0, 0xffc0, move_sr_ea);
		MODE(0x4100, 0xf1c0, chk_l);
		MODE(0x4180, 0xf1c0, chk_w);
		MODE(0x41c0, 0xf1c0, lea);
		IS3(0x4200, 0xffc0, clr);
		MODE(0x42c0, 0xffc0, move_ccr_ea);
		IS3(0x4400, 0xffc0, neg);
		MODE(0x44c0, 0xffc0, move_ea_ccr);
		IS3(0x4600, 0xffc0, _not);
		MODE(0x46c0, 0xffc0, move_ea_sr);
		MODE(0x4800, 0xffc0, nbcd);
		add(0x4808, 0xfff8, P(link_l)); // overwrite nbcd
		MODE(0x4840, 0xffc0, pea);
		add(0x4840, 0xfff8, P(swap)); // overwrite pea
		add(0x4848, 0xfff8, P(bkpt)); // overwrite pea
		MODES(0x4880, 0xffc0, movem_rm, 1);
		add(0x4880, 0xfff8, P(ext_w)); // overwrite movem_rm
		MODES(0x48c0, 0xffc0, movem_rm, 2);
		add(0x48c0, 0xfff8, P(ext_l));
		add(0x49c0, 0xfff8, P(ext_bl));
		IS3(0x4a00, 0xffc0, tst);
		MODE(0x4ac0, 0xffc0, tas);
		MODE(0x4c00, 0xffc0, mul_l);
		MODE(0x4c40, 0xffc0, div_l);
		MODES(0x4c80, 0xffc0, movem_mr, 1);
		MODES(0x4cc0, 0xffc0, movem_mr, 2);
		add(0x4e40, 0xfff0, P(trap));
		add(0x4e50, 0xfff8, P(link_w));
		add(0x4e58, 0xfff8, P(unlk));
		add(0x4e68, 0xfff8, P(move_usp_an));
		add(0x4e60, 0xfff8, P(move_an_usp));
		add(0x4e70, 0xffff, P(reset));
		add(0x4e71, 0xffff, P(nop));
		add(0x4e72, 0xffff, P(stop));
		add(0x4e73, 0xffff, P(rte));
		add(0x4e74, 0xffff, P(rtd));
		add(0x4e75, 0xffff, P(rts));
		add(0x4e76, 0xffff, P(trapv));
		add(0x4e77, 0xffff, P(rtr));
		add(0x4e7a, 0xfffe, P(movec));
		MODE(0x4e80, 0xffc0, jsr);
		MODE(0x4ec0, 0xffc0, jmp);
		IS3(0x5000, 0xf1c0, addq);
		add(0x5008, 0xf138, P(addqa));
		IS3(0x5100, 0xf1c0, subq);
		add(0x5108, 0xf138, P(subq_a));
		MODES(0x50c0, 0xffc0, scc, 0);
		MODES(0x51c0, 0xffc0, scc, 1);
		MODES(0x52c0, 0xffc0, scc, 2);
		MODES(0x53c0, 0xffc0, scc, 3);
		MODES(0x54c0, 0xffc0, scc, 4);
		MODES(0x55c0, 0xffc0, scc, 5);
		MODES(0x56c0, 0xffc0, scc, 6);
		MODES(0x57c0, 0xffc0, scc, 7);
		MODES(0x58c0, 0xffc0, scc, 8);
		MODES(0x59c0, 0xffc0, scc, 9);
		MODES(0x5ac0, 0xffc0, scc, 10);
		MODES(0x5bc0, 0xffc0, scc, 11);
		MODES(0x5cc0, 0xffc0, scc, 12);
		MODES(0x5dc0, 0xffc0, scc, 13);
		MODES(0x5ec0, 0xffc0, scc, 14);
		MODES(0x5fc0, 0xffc0, scc, 15);
		CC(0x50c8, 0xfff8, dbcc); // overwrite scc
		CC(0x50fa, 0xfffe, trapcc);
		CC(0x50fc, 0xffff, trapcc);
		BCC(0x6000, 0xff00, 0);
		BCC(0x6000, 0xffff, 2);
		BCC(0x60ff, 0xffff, 4);
		add(0x6100, 0xff00, PI(bsr, 0)); // overwrite bcc
		add(0x6100, 0xffff, PI(bsr, 2)); // overwrite bcc
		add(0x61ff, 0xffff, PI(bsr, 4)); // overwrite bcc
		add(0x7000, 0xf100, P(moveq));
		IS3(0x8000, 0xf1c0, or_ea_d);
		MODE(0x80c0, 0xf1c0, divu_w);
		IS3(0x8100, 0xf1c0, or_d_ea);
		add(0x8100, 0xf1f0, P(sbcd_r)); // overwrite or_d_ea
		add(0x8108, 0xf1f8, P(sbcd_m)); // overwrite or_d_ea
		MODE(0x81c0, 0xf1c0, divs_w);
		IS3(0x9000, 0xf1c0, sub_ea_d);
		MODE(0x90c0, 0xf1c0, suba_w);
		IS3(0x9100, 0xf1c0, sub_d_ea);
		S3(0x9100, 0xf1f8, subx_r); // overwrite sub_d_ea
		S3(0x9108, 0xf1f8, subx_m); // overwrite sub_d_ea
		MODE(0x91c0, 0xf1c0, suba_l);
		add(0xa000, 0xf000, P(a_line));
		IS3(0xb000, 0xf1c0, cmp);
		MODE(0xb0c0, 0xf1c0, cmpa_w);
		IS3(0xb100, 0xf1c0, eor_d_ea);
		S3(0xb108, 0xf1f8, cmpm); // overwrite eor_d_ea
		MODE(0xb1c0, 0xf1c0, cmpa_l);
		IS3(0xc000, 0xf1c0, and_ea_d);
		MODE(0xc0c0, 0xf1c0, mulu_w);
		IS3(0xc100, 0xf1c0, and_d_ea);
		add(0xc140, 0xf1f8, P(exg_dd));
		add(0xc148, 0xf1f8, P(exg_aa));
		add(0xc188, 0xf1f8, P(exg_da));
		add(0xc100, 0xf1f8, P(abcd_r)); // overwrite and_d_ea
		add(0xc108, 0xf1f8, P(abcd_m)); // overwrite and_d_ea
		MODE(0xc1c0, 0xf1c0, muls_w);
		IS3(0xd000, 0xf1c0, add_ea_d);
		MODE(0xd0c0, 0xf1c0, adda_w);
		IS3(0xd100, 0xf1c0, add_d_ea);
		MODE(0xd1c0, 0xf1c0, adda_l);
		S3(0xd100, 0xf1f8, addx_r); // overwrite add_d_ea
		S3(0xd108, 0xf1f8, addx_m); // overwrite add_d_ea
		add(0xe000, 0xf0c0, PIS(sftrot, 0, 0));
		add(0xe040, 0xf0c0, PIS(sftrot, 0, 1));
		add(0xe080, 0xf0c0, PIS(sftrot, 0, 2));
		MODES(0xe0c0, 0xf0c0, sftrot, 3);
		MODE(0xe8c0, 0xf8c0, bitfield);
		add(0xf000, 0xf000, P(f_line));
	}
	void add(uint16_t op, uint16_t mask, pf_t f) {
		int lim = (op & 0xf000) + 0x1000;
		for (int i = op & 0xf000; i < lim; i++)
			if ((i & mask) == op) fn[i] = f;
	}
	void exec1(M68000 *mpu, uint16_t op) const { fn[op](mpu, op); }
	pf_t fn[0x10000];
	inline static union { pmf_t pmf; pf_t p; } cnv; // not portable
} insn;

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
// M: 0-6:0-6 7:none 8:7-0 9:7-1 10:7-2 11:7-3 12:7-4 13:3,REG7 14:4,REG7 15:none
// S: 0...byte 1...word 2...long
template<int RW, int M, int S, typename F> M68000::u32 M68000::ea(int reg, F func) {
	auto exmode = [&](u32 base) {
		u16 ext = fetch2();
		u32 t;
		if constexpr (MPU_TYPE >= 68020) {
			auto idx = [&]{
				u32 t = (ext & 0x8000 ? a : d)[ext >> 12 & 7];
				return (ext & 0x800 ? t : (s16)t) << ext >> 9 & 3;
			};
			auto disp = [&](int sw)->u32 {
				switch (sw & 3) {
					case 2: return fetch2();
					case 3: return fetch4();
				}
				return 0;
			};
			if (ext & 0x100) { // full extension
				t = ext & 0x80 ? 0 : base; // BS
				if (ext & 0x40) // IS=1
					t = ext & 7 ? get4(t + disp(ext >> 4)) + disp(ext) : t + disp(ext >> 4) + disp(ext);
				else { // IS=0
					t += disp(ext >> 4);
					t = (ext & 7 ? ext & 4 ? get4(t) + idx() : get4(t + idx()) : t + idx()) + disp(ext);
				}
			}
			else t = base + idx() + s8(ext & 0xff); // brief extension
		}
		else {
			t = (ext & 0x8000 ? a : d)[ext >> 12 & 7];
			t = ext & 0x800 ? t : (s16)t;
			t += base + s8(ext & 0xff);
		}
		return t;
	};
	u32 adr = 0, data = 0;
	reg &= 7;
	if constexpr (M == 2) adr = a[reg];
	if constexpr (M == 3 || M == 13) { // (An)+
		adr = a[reg];
		if constexpr (RW != 0) {
			if constexpr (M == 13 && S == 0) a[reg] += 2;
			else a[reg] += 1 << S;
		}
	}
	if constexpr (M == 4 || M == 14) { // -(An)
		if constexpr (RW != 0)
			if constexpr (M == 14 && S == 0) adr = a[reg] -= 2;
			else adr = a[reg] -= 1 << S;
		else adr = a[reg];
	}
	if constexpr (M == 5) adr = a[reg] + fetch2(); // (d16,An)
	if constexpr (M == 6) adr = exmode(a[reg]);
	if constexpr (M == 8) adr = fetch2(); // (abs).W
	if constexpr (M == 9) adr = fetch4(); // (abs).L
	if constexpr (M == 10) { adr = pc; adr += fetch2(); } // (d16,PC)
	if constexpr (M == 11) adr = exmode(pc); // (d8,PC,Xn)
//
	if constexpr (RW & 1) {
		if constexpr (M == 0) data = d[reg]; // Dn
		else if constexpr (M == 1) data = a[reg]; // An
		else if constexpr (M == 12) data = getImm<S>(); // #imm
		else data = ld<S>(adr);
	}
	if constexpr (RW == 1) func(data);
	if constexpr (RW == 2) data = func();
	if constexpr (RW == 3) data = func(data);
	if constexpr ((RW & 2) == 2) {
		if constexpr (M == 0) stD<S>(reg, data); // dst Dn
		else if constexpr (M == 1) stA(reg, data); // dst An
		else st<S>(adr, data);
	}
	return adr;
}

template<int M, int S> void M68000::movem_rm(u16 op) {
	const u32 ofs = S << 1, list = fetch2();
	u32 i, adr = ea<0, M, S>(op, []{});
	if constexpr (M == 4 || M == 14) {
		for (i = 0; i < 8; i++)
			if (list & 1 << i) {
				if constexpr (MPU_TYPE >= 68020) { a[R0] = adr -= ofs; st<S>(adr, a[7 - i]); }
				else st<S>(adr -= ofs, a[7 - i]);
			}
		for (i = 0; i < 8; i++)
			if (list & 0x100 << i) st<S>(adr -= ofs, d[7 - i]);
		a[R0] = adr;
	}
	else {
		for (i = 0; i < 8; i++)
			if (list & 1 << i) { st<S>(adr, d[i]); adr += ofs; }
		for (i = 0; i < 8; i++)
			if (list & 0x100 << i) { st<S>(adr, a[i]); adr += ofs; }
	}
}

template<int M, int S> void M68000::movem_mr(u16 op) {
	const u32 ofs = S << 1, list = fetch2();
	u32 i, adr = ea<0, M, S>(op, []{});
	if constexpr (S == 1) {
		for (i = 0; i < 8; i++)
			if (list & 1 << i) { stD<2>(i, (s16)ld2(adr)); adr += ofs; }
		for (i = 0; i < 8; i++)
			if (list & 0x100 << i) { stA(i, (s16)ld2(adr)); adr += ofs; };
	}
	else {
		for (i = 0; i < 8; i++)
			if (list & 1 << i) { stD<2>(i, ld4(adr)); adr += ofs; };
		for (i = 0; i < 8; i++)
			if (list & 0x100 << i) { stA(i, ld4(adr)); adr += ofs; };
	}
	if constexpr (M == 3 || M == 13) a[R0] = adr; // write back if (An)+
}

void M68000::movec(u16 op) {
	u16 t = fetch2();
	if (t >= 8) t -= 0x7f8;
	if (t < 16) {
		if (op & 1) cr[t] = (t & 0x8000 ? d : a)[t >> 12 & 7];
		else (t & 0x8000 ? d : a)[t >> 12 & 7] = cr[t];
	}
}

void M68000::movep(u16 op) {
	u32 t, adr = a[R0] + fetch2();
	switch (op & 0xc0) {
		case 0: // movep.w (d16,Ay),Dx
			t = ld1(adr) << 8;
			stD<1>(R9, t | ld1(adr + 2));
			break;
		case 0x40: // movep.l (d16,Ay),Dx
			t = ld1(adr) << 24;
			t |= ld1(adr + 2) << 16;
			t |= ld1(adr + 4) << 8;
			stD<2>(R9, t | ld1(adr + 6));
			break;
		case 0x80: // movep.w Dx,(d16,Ay)
			t = d[R9];
			st1(adr, t >> 8);
			st1(adr + 2, t);
			break;
		default: // movep.l Dx,(d16,Ay)
			t = d[R9];
			st1(adr, t >> 24);
			st1(adr + 2, t >> 16);
			st1(adr + 4, t >> 8);
			st1(adr + 6, t);
			break;
	}
}

template<int M> void M68000::mul_l(u16 op) {
	u32 t = fetch2(), f64 = t & 0x400;
	u32 &dq = d[t >> 12 & 7], &dr = d[t & 7];
	if (t & 0x800) // muls.l
		ea<1, M, 2>(op, [&](s32 v) {
			s64 ts = (s32)dq * (s64)v;
			fmul(dq, v, f64 ? ts >> 32 : (s32)ts, f64 ? 0 : 1);
			dq = (s32)ts;
			if (f64) dr = ts >> 32;
		});
	else // mulu.l
		ea<1, M, 2>(op, [&](u32 v) {
			u64 tu = dq * (u64)v;
			fmul(dq, v, f64 ? tu >> 32 : (u32)tu, f64 ? 0 : 2);
			dq = (u32)tu;
			if (f64) dr = tu >> 32;
		});
}

template<int M> void M68000::divs_w(u16 op) { // divs.w
	auto divs = [&](s16 s, s32 d)->s32 {
		if (s) {
			s32 q = d / s;
			if (q == (s16)q) return flogic(d % s << 16 | (q & 0xffff), 1);
			else fset0(VS);
		}
		else { fset0(C0); Trap(5); }
		return d;
	};
	ea<1, M, 1>(op, [&](s16 v) { stD<2>(R9, divs(v, d[R9])); });
}

template<int M> void M68000::divu_w(u16 op) { // divu.w
	auto divu = [&](u16 s, u32 d) {
		if (s) {
			u32 q = d / s;
			if (q == (u16)q) return flogic(d % s << 16 | (q & 0xffff), 1);
			else fset0(VS);
		}
		else { fset0(C0); Trap(5); }
		return d;
	};
	ea<1, M, 1>(op, [&](u16 v) { stD<2>(R9, divu(v, d[R9])); });
}

template<int M> void M68000::div_l(u16 op) {
	u32 t = fetch2(), f64 = t & 0x400;
	u32 &dq = d[t >> 12 & 7], &dr = d[t & 7];
	if (t & 0x800) // divs.l
		ea<1, M, 2>(op, [&](s32 s) {
			if (s) {
				s64 q = dq;
				if (f64) q |= (s64)dr << 32;
				dr = q % s;
				q /= s;
				if (q == (s32)q) dq = flogic((s32)q, 2);
				else fset0(VS);
			}
			else { fset0(C0); Trap(5); }
		});
	else // divu.l
		ea<1, M, 2>(op, [&](u32 s) {
			if (s) {
				u64 q = dq;
				if (f64) q |= (u64)dr << 32;
				dr = q % s;
				q /= s;
				if (q == (u32)q) dq = flogic((u32)q, 2);
				else fset0(VS);
			}
			else { fset0(C0); Trap(5); }
		});
}

template<int W, int M, typename F> void M68000::bitop(u16 op, F func) {
	u32 m;
	if constexpr ((W & 2) != 0) m = d[R9];
	else m = fetch2();
	if constexpr (!M) m = 1 << (m & 0x1f);
	else m = 1 << (m & 7);
	if constexpr (W & 1) { ea<3, M, M ? 0 : 2>(op, [&](u32 v) { fbtst(v & m, M ? 0 : 2); return func(v, m); }); }
	else ea<1, M, M ? 0 : 2>(op, [&](u32 v) { fbtst(v & m, M ? 0 : 2); });
}

template<int M> void M68000::bitfield(u16 op) { // M68000PRM.pdf page 1-29
	u32 i, adr = 0, t = fetch2(), ofs = t & 0x800 ? d[t >> 6 & 7] : t >> 6 & 0x1f;
	u32 width = ((t & 0x20 ? d[t & 7] : t) - 1 & 0x1f) + 1, bofs = 0x40 - (ofs & 0x1f) - width;
	u64 data;
	if constexpr (M != 0) {
		adr = ea<0, M, 0>(op, []{}) + (ofs >> 3);
		data = (u64)ld4(adr) << 32 | ld4(adr + 4);
	}
	else data = (u64)d[R0] << 32;
	auto ext = [&]{ return fbf(u32(data >> bofs & (1LL << width) - 1), width); };
	auto mask = [&]{ return ((1LL << width) - 1) << bofs; };
	switch (op & 0x700) {
		default: ext(); return; // bftst
		case 0x100: stD<2>(t >> 12 & 7, ext()); return; // bfextu
		case 0x200: ext(); data ^= mask(); break; // bfchg
		case 0x300: stD<2>(t >> 12 & 7, s32(ext() << (32 - width)) >> (32 - width)); return; // bfexts
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
	if constexpr (M != 0) {
		st4(adr, data >> 32);
		st4(adr + 4, (u32)data);
	}
	else d[R0] = data >> 32;
}

template <int M, int S> void M68000::sftrot(u16 op) {
	static constexpr u32 dm[] = {
		XSR | NS | ZS | V0 | CSR, XSR | NS | ZS | V0 | CSR, XSR | NS | ZS | V0 | CXR, NS | ZS | V0 | CSR,
		XSL | NS | ZS | VSL | CSL, XSL | NS | ZS | V0 | CSL, XSL | NS | ZS | V0 | CXL, NS | ZS | V0 | CSL
	};
	int sop = (op >> 6 & 4) | (op >> (S < 3 ? 3 : 9) & 3), bits = S < 3 ? 8 << S : 16;
	auto f = [&](auto opfunc) {
		if constexpr (S < 3) { // register
			if (u32 v = d[R0], s = op & 0x20 ? d[R9] & 0x3f : ((op >> 9) - 1 & 7) + 1; s) {
				if constexpr (S == 0) v = sop & 3 ? (u8)v : (s8)v;
				if constexpr (S == 1) v = sop & 3 ? (u16)v : (s16)v;
				s = sop & 2 ? (sop & 3) == 2 ? s % ((8 << S) + 1) : s & (8 << S) - 1 : s > bits ? bits : s;
				stD<S>(R0, fset(dm[sop], s, v, opfunc(v, s), S));
			}
			else fset2(NS | ZS | V0 | C0, v, S);
		}
		else ea<3, M, 1>(op, [&](u32 v) { return fset(dm[sop], 1, v, opfunc(v, 1), 1); }); // memory
	};
	switch (sop) {
		case 0: // asr
			f([&](u32 v, u32 s) { return v >> s | (v >> (bits - 1) & 1 ? -1 << (bits - s) : 0); });
			break;
		case 1: // lsr
			f([&](u32 v, u32 s) { return v >> s; });
			break;
		case 2: // roxr
			f([&](u32 v, u32 s) { u32 t = bits + 1 - s; return v >> s | (t < 32 ? v << t : 0) | X() << (bits - s); });
			break;
		case 3: // ror
			f([&](u32 v, u32 s) { return v >> s | v << (bits - s); });
			break;
		default: // asl/lsl
			f([&](u32 v, u32 s) { return v << s; });
			break;
		case 6: // roxl
			f([&](u32 v, u32 s) { u32 t = bits + 1 - s; return v << s | (t < 32 ? v >> t : 0) | X() << (s - 1); });
			break;
		case 7: // rol
			f([&](u32 v, u32 s) { return v << s | v >> (bits - s); });
			break;
	}
}

template<int W, int M> void M68000::bcd(u16 op) {
	auto add = [&](u8 x, u8 y) {
		s16 l = (y & 0xf) + (x & 0xf) + X(), cl = l >= 0xa, u = (y & 0xf0) + (x & 0xf0) + (cl << 4), cu = u >= 0xa0;
		return fbcd(cu, u - (cu ? 0xa0 : 0) + l - (cl ? 0xa : 0));
	};
	auto sub = [&](u8 x, u8 y) {
		s16 l = (y & 0xf) - (x & 0xf) - X(), cl = l < 0, u = (y & 0xf0) - (x & 0xf0) - (cl << 4), cu = u < 0;
		return fbcd(cu, u + (cu ? 0xa0 : 0) + l + (cl ? 0xa : 0));
	};
	if constexpr (W == 2) ea<3, M, 0>(op, [&](u8 v) { return sub(v, 0); }); // nbcd
	else { // abcd / sbcd
		u8 r, x, y;
		if constexpr (!M) { x = d[R9]; y = d[R0]; }
		else { x = ld1(--a[R9]); y = ld1(--a[R0]); }
		if constexpr (!W) r = add(x, y);
		else r = sub(x, y);
		if constexpr (!M) stD<0>(R9, r);
		else st1(a[R9], r);
	}
}

template <int C, int S> void M68000::bcc(u16 op) {
	if (cond<C>()) {
		u32 t = pc;
		if constexpr (S == 2) t += fetch2();
		else if constexpr (S == 4) t += fetch4();
		else t += s8(op & 0xff);
		pc = t;
	}
	else pc += S;
}

template <int S> void M68000::bsr(u16 op) {
	u32 t = pc;
	if constexpr (S == 2) t += fetch2();
	else if constexpr (S == 4) t += fetch4();
	else t += s8(op & 0xff);
	push4(pc);
	pc = t;
}

template<int C> void M68000::dbcc(u16 op) {
	if (!cond<C>()) {
		s16 s = d[R0] - 1;
		stD<1>(R0, s);
		pc += s != -1 ? fetch2() - 2 : 2;
	}
	else pc += 2;
}

void M68000::rte(u16) {
	u32 t = pop2();
	pc = pop4();
	SetSR(t);
	if constexpr (MPU_TYPE >= 68020)
		if ((pop2() & 0xf000) == 0x2000) a[7] += 4;
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
//		if (stopf) return 0;
#if M68000_TRACE
		tracep->pc = pc;
		tracep->index = 0;
#endif
		insn.exec1(this, fetch2());
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
		clock += 10; // average 68000 CPI (provisional)
	} while (/* !stopf && */clock < n);
	return stopf ? 0 : clock - n;
}

template<int C> int M68000::cond() {
	if constexpr (C == 1) return false;
	if constexpr (C == 2) return !ResolvC() && !ResolvZ();
	if constexpr (C == 3) return ResolvC() || ResolvZ();
	if constexpr (C == 4) return !ResolvC();
	if constexpr (C == 5) return ResolvC();
	if constexpr (C == 6) return !ResolvZ();
	if constexpr (C == 7) return ResolvZ();
	if constexpr (C == 8) return !ResolvV();
	if constexpr (C == 9) return ResolvV();
	if constexpr (C == 10) return !ResolvN();
	if constexpr (C == 11) return ResolvN();
	if constexpr (C == 12) return ~(ResolvN() >> LN ^ ResolvV() >> LV) & 1;
	if constexpr (C == 13) return (ResolvN() >> LN ^ ResolvV() >> LV) & 1;
	if constexpr (C == 14) return ~(ResolvZ() >> LZ | (ResolvN() >> LN ^ ResolvV() >> LV)) & 1;
	if constexpr (C == 15) return (ResolvZ() >> LZ | (ResolvN() >> LN ^ ResolvV() >> LV)) & 1;
	return true;
};

#define MSB_N	((8 << p->size) - 1)
#define IS0		!(p->r & u32((1LL << (8 << p->size)) - 1))

int M68000::ResolvC() {
	u32 sw = 0;
	FlagDecision *p;
	for (p = fp - 1; p >= fbuf && !(sw = p->dm & 0xf); p--)
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
		case FXL:
			if (!p->s) return ResolvX() >> LX << LC;
			//no break
		case FSL:
			return p->s ? (p->d >> ((8 << p->size) - p->s) & 1) << LC : 0;
		case FXR:
			if (!p->s) return ResolvX() >> LX << LC;
			//no break
		case FSR:
			return p->s ? (p->d >> (p->s - 1) & 1) << LC : 0;
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
			if (p->size/*type*/ == 1) { s64 ts = (s64)(s32)p->s * (s32)p->d; return ts != (s32)ts; }
			else if (p->size/*type*/ == 2) { u64 tu = (u64)p->s * p->d; return tu != (u32)tu; }
			return 0;
		case FSL:
		{ // ex) count=4,bits=8  V=0 if Mmmmmxxx all of m equal M
			u32 m = ((1 << p->s) - 1) << (MSB_N - p->s);
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
			return (!p->s || !p->d) << LZ; // not use p->r because it's 32bit
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
			return (p->r >> MSB_N & 1) << LN;
		case FMUL:
			return (p->r >> 31 & 1) << LN;
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

void M68000::undef(u16 op) {
	fprintf(stderr, "undefined instruction: PC=%06x OP=%04x\n", pc - 2, op);
#if M68000_TRACE
	StopTrace();
#endif
	exit(1);
}
