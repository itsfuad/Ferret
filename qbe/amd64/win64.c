#include "all.h"

/* Windows x64 calling convention summary:
 * - Four register argument slots: RCX, RDX, R8, R9 (GPR) or XMM0..3 (FP) chosen per argument type.
 * - Caller must reserve 32 bytes of shadow space for every call.
 * - Return: scalars in RAX/XMM0. Aggregates > 8 bytes use a hidden sret pointer in RCX; callee returns that pointer in RAX.
 * - We currently treat all XMM registers as caller-saved (safe, but not optimal) to avoid changing the shared register model.
 * - Varargs use the shadow space to access register arguments.
 */

typedef struct WArgClass WArgClass;

struct WArgClass {
	int inmem;
	int align;
	uint size;
	int cls; /* Kw/Kl/Ks/Kd */
};

static int win64_gpr_arg[] = {RCX, RDX, R8, R9};
static int win64_fpr_arg[] = {XMM0, XMM1, XMM2, XMM3};

enum { Win64Shadow = 32 };

int amd64_win64_rsave[] = {
	RCX, RDX, R8, R9, R10, R11, RAX,
	XMM0, XMM1, XMM2, XMM3, XMM4, XMM5,
	-1
};

/* Only GPR callee-saves are modeled to match the shared emitter/reg model. */
int amd64_win64_rclob[] = {
	RBX, RBP, RSI, RDI, R12, R13, R14, R15,
	XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
	-1
};

static uint
win64_align8(uint n)
{
	return (n + 7u) & -8u;
}

static uint
win64_align16(uint n)
{
	return (n + 15u) & ~15u;
}

static int
win64_stackslot(uint off)
{
	return -(int)((off + 16u) / 4u);
}

static int
win64_storeop(int cls)
{
	static int st[] = { Ostorew, Ostorel, Ostores, Ostored };

	if (cls < Kw || cls > Kd)
		die("invalid win64 store class");
	return st[cls];
}

static int
win64_popcnt4(int m)
{
	return (m & 1) + ((m >> 1) & 1) + ((m >> 2) & 1) + ((m >> 3) & 1);
}

static void
win64_typclass(WArgClass *a, Typ *t)
{
	uint al;

	a->inmem = 0;
	a->cls = Kl;

	al = 1u << t->align;
	if (al > 8)
		al = 8;
	a->size = win64_align8(t->size);
	a->align = t->align;

	if (t->dark || a->size == 0 || a->size > 8) {
		a->inmem = 1;
		a->cls = Kl;
		return;
	}
	if (a->size <= 4)
		a->cls = Kw;
}

static int
argclass(Ins *i0, Ins *i1, WArgClass *ac, WArgClass *aret, Ref *env)
{
	int slot, nslot;
	Ins *i;
	WArgClass *a;

	slot = (aret && aret->inmem) ? 1 : 0;
	nslot = 0;
	for (i=i0, a=ac; i<i1; i++, a++, slot++) {
		switch (i->op) {
		case Oarg:
		case Opar:
			a->inmem = slot >= 4;
			a->align = 3;
			a->size = 8;
			a->cls = i->cls;
			if (a->inmem)
				nslot++;
			break;
		case Oargc:
		case Oparc:
			win64_typclass(a, &typ[i->arg[0].val]);
			if (slot >= 4)
				nslot += a->inmem ? 1 : (a->size + 7) / 8;
			break;
		case Oarge:
			slot--;
			if (env)
				*env = i->arg[0];
			break;
		case Opare:
			slot--;
			if (env)
				*env = i->to;
			break;
		}
	}

	return nslot;
}

bits
amd64_win64_retregs(Ref r, int p[2])
{
	bits b;
	int ni, nf;

	assert(rtype(r) == RCall);
	b = 0;
	ni = r.val & 3;
	nf = (r.val >> 2) & 3;
	if (ni >= 1)
		b |= BIT(RAX);
	if (nf >= 1)
		b |= BIT(XMM0);
	if (p) {
		p[0] = ni;
		p[1] = nf;
	}
	return b;
}

bits
amd64_win64_argregs(Ref r, int p[2])
{
	bits b;
	int j, gpm, fpm, ra;

	assert(rtype(r) == RCall);
	b = 0;
	gpm = (r.val >> 4) & 15;
	fpm = (r.val >> 8) & 15;
	ra = (r.val >> 12) & 1;
	for (j=0; j<4; j++)
		if (gpm & (1 << j))
			b |= BIT(win64_gpr_arg[j]);
	for (j=0; j<4; j++)
		if (fpm & (1 << j))
			b |= BIT(win64_fpr_arg[j]);
	if (ra)
		b |= BIT(RAX);
	if (p) {
		p[0] = win64_popcnt4(gpm) + ra;
		p[1] = win64_popcnt4(fpm);
	}
	return b;
}

static Ref
win64_rarg(int cls, int slot)
{
	if (KBASE(cls) == 0)
		return TMP(win64_gpr_arg[slot]);
	return TMP(win64_fpr_arg[slot]);
}

static void
selret(Blk *b, Fn *fn)
{
	int j, k, ca;
	Ref r0;
	WArgClass aret;

	j = b->jmp.type;
	if (!isret(j) || j == Jret0)
		return;

	r0 = b->jmp.arg;
	b->jmp.type = Jret0;
	ca = 0;

	if (j == Jretc) {
		win64_typclass(&aret, &typ[fn->retty]);
		if (aret.inmem) {
			assert(rtype(fn->retr) == RTmp);
			emit(Ocopy, Kl, TMP(RAX), fn->retr, R);
			blit(fn->retr, 0, r0, aret.size, fn);
			ca = 1;
		} else {
			emit(Oload, aret.cls, TMP(RAX), r0, R);
			ca = 1;
		}
	} else {
		k = j - Jretw;
		if (KBASE(k) == 0) {
			emit(Ocopy, k, TMP(RAX), r0, R);
			ca = 1;
		} else {
			emit(Ocopy, k, TMP(XMM0), r0, R);
			ca = 1 << 2;
		}
	}

	b->jmp.arg = CALL(ca);
}

static void
selcall(Fn *fn, Ins *i0, Ins *i1)
{
	Ins *i;
	WArgClass *ac, *a, aret;
	int ca, slot, gpm, fpm, envc, varc;
	uint off, stk;
	Ref r, r1, rstk, retbuf, env;
	int al;
	int hasstk, sslot;

	varc = i1->op == Ovacall;
	env = R;
	ac = alloc((i1-i0) * sizeof ac[0]);
	if (!req(i1->arg[1], R)) {
		assert(rtype(i1->arg[1]) == RType);
		win64_typclass(&aret, &typ[i1->arg[1].val]);
	} else {
		memset(&aret, 0, sizeof aret);
		aret.inmem = 0;
		aret.size = 8;
		aret.cls = i1->cls;
	}

	argclass(i0, i1, ac, aret.inmem ? &aret : 0, &env);

	hasstk = 0;
	off = Win64Shadow;
	sslot = (!req(i1->arg[1], R) && aret.inmem) ? 1 : 0;
	for (i=i0, a=ac; i<i1; i++, a++) {
		if (i->op == Oarge)
			continue;
		if (sslot >= 4) {
			off += 8;
			hasstk = 1;
		}
		sslot++;
	}
	stk = win64_align16(off);

	ca = 0;
	slot = 0;
	gpm = 0;
	fpm = 0;
	retbuf = R;

	if (!req(i1->arg[1], R)) {
		retbuf = newtmp("abi", Kl, fn);
		if (aret.inmem) {
			ca |= 1; /* gp return */
			gpm |= 1;
			slot++; /* hidden sret consumes RCX */
		} else if (KBASE(aret.cls) == 0) {
			ca |= 1;
		} else {
			ca |= 1 << 2;
		}
	} else {
		if (KBASE(i1->cls) == 0)
			ca |= 1;
		else
			ca |= 1 << 2;
	}

	for (i=i0, a=ac; i<i1; i++, a++) {
		switch (i->op) {
		case Oarg:
			if (slot < 4 && !a->inmem) {
				if (KBASE(i->cls) == 0)
					gpm |= 1 << slot;
				else
					fpm |= 1 << slot;
			}
			slot++;
			break;
		case Oargc:
			if (slot < 4) {
				if (a->inmem) {
					gpm |= 1 << slot;
				} else {
					if (a->size > 8)
						err("win64 abi cannot pass aggregates >8 bytes in registers");
					if (KBASE(a->cls) == 0)
						gpm |= 1 << slot;
					else
						fpm |= 1 << slot;
				}
			}
			slot++;
			break;
		case Oarge:
			break;
		default:
			die("unreachable");
		}
	}

	envc = !req(R, env);
	ca |= (gpm << 4) | (fpm << 8);
	if (envc)
		ca |= 1 << 12;

	r = R;
	if (hasstk || varc)
		r = newtmp("abi", Kl, fn);

	if (stk) {
		rstk = getcon(-(int64_t)stk, fn);
		emit(Osalloc, Kl, R, rstk, R);
	}

	if (!req(i1->arg[1], R)) {
		if (aret.inmem) {
			emit(Ocopy, Kl, i1->to, TMP(RAX), R);
		} else {
			Ref tmp;

			tmp = newtmp("abi", aret.cls, fn);
			if (KBASE(aret.cls) == 0)
				emit(win64_storeop(aret.cls), 0, R, tmp, retbuf);
			else
				emit(win64_storeop(aret.cls), 0, R, tmp, retbuf);
			if (KBASE(aret.cls) == 0)
				emit(Ocopy, aret.cls, tmp, TMP(RAX), R);
			else
				emit(Ocopy, aret.cls, tmp, TMP(XMM0), R);
			emit(Ocopy, Kl, i1->to, retbuf, R);
		}
	} else {
		if (KBASE(i1->cls) == 0)
			emit(Ocopy, i1->cls, i1->to, TMP(RAX), R);
		else
			emit(Ocopy, i1->cls, i1->to, TMP(XMM0), R);
	}

	emit(Ocall, i1->cls, R, i1->arg[0], CALL(ca));

	if (envc)
		emit(Ocopy, Kl, TMP(RAX), env, R);

	slot = (!req(i1->arg[1], R) && aret.inmem) ? 1 : 0;
	off = Win64Shadow;

	for (i=i0, a=ac; i<i1; i++, a++) {
		switch (i->op) {
		case Oarg:
			if (slot < 4 && !a->inmem) {
				emit(Ocopy, i->cls, win64_rarg(i->cls, slot), i->arg[0], R);
				if (varc) {
					r1 = newtmp("abi", Kl, fn);
					emit(win64_storeop(i->cls), 0, R, i->arg[0], r1);
					emit(Oadd, Kl, r1, r, getcon(slot * 8, fn));
				}
			} else {
				r1 = newtmp("abi", Kl, fn);
				emit(win64_storeop(i->cls), 0, R, i->arg[0], r1);
				emit(Oadd, Kl, r1, r, getcon(off, fn));
				off += 8;
			}
			slot++;
			break;
		case Oargc:
			if (a->inmem) {
				if (slot < 4) {
					emit(Ocopy, Kl, win64_rarg(Kl, slot), i->arg[1], R);
					if (varc) {
						r1 = newtmp("abi", Kl, fn);
						emit(Ostorel, 0, R, i->arg[1], r1);
						emit(Oadd, Kl, r1, r, getcon(slot * 8, fn));
					}
				} else {
					r1 = newtmp("abi", Kl, fn);
					emit(Ostorel, 0, R, i->arg[1], r1);
					emit(Oadd, Kl, r1, r, getcon(off, fn));
					off += 8;
				}
			} else if (slot < 4) {
				if (a->size > 8)
					err("win64 abi cannot pass aggregates >8 bytes in registers");
				emit(Oload, a->cls, win64_rarg(a->cls, slot), i->arg[1], R);
				if (varc) {
					Ref tmp;

					tmp = newtmp("abi", a->cls, fn);
					r1 = newtmp("abi", Kl, fn);
					emit(win64_storeop(a->cls), 0, R, tmp, r1);
					emit(Oadd, Kl, r1, r, getcon(slot * 8, fn));
					emit(Oload, a->cls, tmp, i->arg[1], R);
				}
			} else {
				blit(r, off, i->arg[1], a->size, fn);
				off += 8;
			}
			slot++;
			break;
		case Oarge:
			break;
		default:
			die("unreachable");
		}
	}

	if (!req(i1->arg[1], R)) {
		al = aret.align >= 2 ? aret.align - 2 : 0;
		if (aret.inmem)
			emit(Ocopy, Kl, win64_rarg(Kl, 0), retbuf, R);
		emit(Oalloc + al, Kl, retbuf, getcon(aret.size, fn), R);
	}

	if (stk)
		emit(Osalloc, Kl, r, getcon(stk, fn), R);
}

static uint
selpar(Fn *fn, Ins *i0, Ins *i1)
{
	WArgClass *ac, aret, *a;
	Ins *i;
	int slot;
	uint off, vaoff;
	Ref r, env;
	int al;

	env = R;
	ac = alloc((i1-i0) * sizeof ac[0]);
	curi = &insb[NIns];
	if (fn->retty >= 0)
		win64_typclass(&aret, &typ[fn->retty]);
	else {
		memset(&aret, 0, sizeof aret);
		aret.inmem = 0;
	}

	argclass(i0, i1, ac, aret.inmem ? &aret : 0, &env);

	slot = 0;
	off = Win64Shadow;

	if (fn->retty >= 0 && aret.inmem) {
		fn->retr = TMP(RCX);
		slot++;
	}

	for (i=i0, a=ac; i<i1; i++, a++) {
		switch (i->op) {
		case Opar:
			if (slot < 4 && !a->inmem) {
				r = win64_rarg(i->cls, slot);
				emit(Ocopy, i->cls, i->to, r, R);
			} else {
				emit(Oload, i->cls, i->to, SLOT(win64_stackslot(off)), R);
				off += 8;
			}
			slot++;
			break;
		case Oparc:
			if (a->inmem) {
				if (slot < 4) {
					r = win64_rarg(Kl, slot);
					emit(Ocopy, Kl, i->to, r, R);
				} else {
					emit(Oload, Kl, i->to, SLOT(win64_stackslot(off)), R);
					off += 8;
				}
			} else if (slot >= 4) {
				fn->tmp[i->to.val].slot = win64_stackslot(off);
				off += 8;
			} else {
				if (a->size > 8)
					err("win64 abi cannot pass aggregates >8 bytes in registers");
				r = win64_rarg(a->cls, slot);
				emit(win64_storeop(a->cls), 0, R, r, i->to);
				al = a->align >= 2 ? a->align - 2 : 0;
				emit(Oalloc + al, Kl, i->to, getcon(a->size, fn), R);
			}
			slot++;
			break;
		case Opare:
			break;
		default:
			die("unreachable");
		}
	}

	if (!req(R, env))
		emit(Ocopy, Kl, env, TMP(RAX), R);

	if (!fn->vararg)
		return 0;

	if (slot < 4)
		vaoff = slot * 8u;
	else
		vaoff = off;

	return vaoff;
}

static void
selvastart(Fn *fn, uint vaoff, Ref ap)
{
	Ref r0;

	r0 = newtmp("abi", Kl, fn);
	emit(Ostorel, Kw, R, r0, ap);
	emit(Oadd, Kl, r0, TMP(RBP), getcon(16 + (int64_t)vaoff, fn));
}

static void
selvaarg(Fn *fn, Ins *i)
{
	Ref ap, cur, next, c8;

	ap = i->arg[0];
	c8 = getcon(8, fn);
	cur = newtmp("abi", Kl, fn);
	next = newtmp("abi", Kl, fn);

	emit(Ostorel, Kw, R, next, ap);
	emit(Oadd, Kl, next, cur, c8);
	emit(Oload, i->cls, i->to, cur, R);
	emit(Oload, Kl, cur, ap, R);
}

void
amd64_win64_abi(Fn *fn)
{
	Blk *b;
	Ins *i, *i0, *ip;
	uint vaoff;
	int n;

	for (b=fn->start; b; b=b->link)
		b->visit = 0;

	for (b=fn->start, i=b->ins; i-b->ins<b->nins; i++)
		if (!ispar(i->op))
			break;
	vaoff = selpar(fn, b->ins, i);
	n = b->nins - (i - b->ins) + (&insb[NIns] - curi);
	i0 = alloc(n * sizeof(Ins));
	ip = icpy(ip = i0, curi, &insb[NIns] - curi);
	ip = icpy(ip, i, &b->ins[b->nins] - i);
	b->nins = n;
	b->ins = i0;

	b = fn->start;
	do {
		if (!(b = b->link))
			b = fn->start;
		if (b->visit)
			continue;
		curi = &insb[NIns];
		selret(b, fn);
		for (i=&b->ins[b->nins]; i!=b->ins; )
			switch ((--i)->op) {
			default:
				emiti(*i);
				break;
			case Ocall:
			case Ovacall:
				for (i0=i; i0>b->ins; i0--)
					if (!isarg((i0-1)->op))
						break;
				selcall(fn, i0, i);
				i = i0;
				break;
			case Oarg:
			case Oargc:
				die("unreachable");
			case Ovastart:
				selvastart(fn, vaoff, i->arg[0]);
				break;
			case Ovaarg:
				selvaarg(fn, i);
				break;
			}
		b->nins = &insb[NIns] - curi;
		idup(&b->ins, curi, b->nins);
	} while (b != fn->start);

	if (debug['A']) {
		fprintf(stderr, "\n> After ABI lowering (win64):\n");
		printfn(fn, stderr);
	}
}
