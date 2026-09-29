/* ---------------------------------------------------------------------------
 * e_speed.c -- Fast speed-dependent emission inventory kernel.
 *
 * Evaluates the EMEP/EEA speed emission-factor equations (as used by
 * ef_ldv_speed() / ef_hdv_speed()) compiled to a small stack machine, and
 * combines them with vehicle flows, link lengths and temporal profiles.
 *
 * Ages sharing the same equation (e.g. several years within one Euro
 * standard) are grouped so the equation is evaluated once per street-hour and
 * reused for every age in the group.  The heavy loop runs with OpenMP.
 * ------------------------------------------------------------------------- */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <math.h>
#include <stdlib.h>

#ifdef _OPENMP
#include <omp.h>
#endif

/* opcodes -- must match R/ef_program.R */
#define OP_CONST  1
#define PUSH_A   10
#define PUSH_B   11
#define PUSH_C   12
#define PUSH_D   13
#define PUSH_E   14
#define PUSH_F   15
#define PUSH_X   16
#define PUSH_V   17
#define OP_ADD   20
#define OP_SUB   21
#define OP_MUL   22
#define OP_DIV   23
#define OP_POW   24
#define FN_EXP   25
#define FN_LOG   26
#define CMP_LT   30
#define CMP_LE   31
#define CMP_GT   32
#define CMP_GE   33
#define CMP_EQ   34
#define CMP_NE   35
#define OP_AND   36
#define OP_OR    37
#define IFELSE   40
#define OP_NEG   41

static double eval_rpn(const int *code, int ncode, const double *cs,
                       double V, const double *cf, double xv) {
  double st[256];
  int sp = 0, pc = 0;
  while (pc < ncode) {
    int op = code[pc++];
    switch (op) {
      case OP_CONST: st[sp++] = cs[code[pc++]]; break;
      case PUSH_A: st[sp++] = cf[0]; break;
      case PUSH_B: st[sp++] = cf[1]; break;
      case PUSH_C: st[sp++] = cf[2]; break;
      case PUSH_D: st[sp++] = cf[3]; break;
      case PUSH_E: st[sp++] = cf[4]; break;
      case PUSH_F: st[sp++] = cf[5]; break;
      case PUSH_X: st[sp++] = xv;   break;
      case PUSH_V: st[sp++] = V;    break;
      case OP_NEG: st[sp - 1] = -st[sp - 1]; break;
      case OP_ADD: { double b = st[--sp], a = st[--sp]; st[sp++] = a + b; } break;
      case OP_SUB: { double b = st[--sp], a = st[--sp]; st[sp++] = a - b; } break;
      case OP_MUL: { double b = st[--sp], a = st[--sp]; st[sp++] = a * b; } break;
      case OP_DIV: { double b = st[--sp], a = st[--sp]; st[sp++] = a / b; } break;
      case OP_POW: { double b = st[--sp], a = st[--sp]; st[sp++] = pow(a, b); } break;
      case FN_EXP: st[sp - 1] = exp(st[sp - 1]); break;
      case FN_LOG: st[sp - 1] = log(st[sp - 1]); break;
      case CMP_LT: { double b = st[--sp], a = st[--sp]; st[sp++] = (a <  b); } break;
      case CMP_LE: { double b = st[--sp], a = st[--sp]; st[sp++] = (a <= b); } break;
      case CMP_GT: { double b = st[--sp], a = st[--sp]; st[sp++] = (a >  b); } break;
      case CMP_GE: { double b = st[--sp], a = st[--sp]; st[sp++] = (a >= b); } break;
      case CMP_EQ: { double b = st[--sp], a = st[--sp]; st[sp++] = (a == b); } break;
      case CMP_NE: { double b = st[--sp], a = st[--sp]; st[sp++] = (a != b); } break;
      case OP_AND: { double b = st[--sp], a = st[--sp]; st[sp++] = (a != 0 && b != 0); } break;
      case OP_OR:  { double b = st[--sp], a = st[--sp]; st[sp++] = (a != 0 || b != 0); } break;
      case IFELSE: { double e = st[--sp], t = st[--sp], c = st[--sp]; st[sp++] = (c != 0) ? t : e; } break;
      default: error("e_speed: invalid opcode %d", op); break;
    }
  }
  return st[0];
}

/*
 * emis_speed_engine
 *  speed  : S x H numeric (column-major), km/h
 *  veh    : S x A numeric (column-major), veh/h
 *  lkm    : S link length (km)
 *  profile: H temporal profile (flattened hours)
 *  code   : concatenated RPN bytecode of the G distinct equations
 *  clen   : code length of each group (G)
 *  consts : concatenated constants
 *  soff   : start index (0-based) of each group's constants (G)
 *  cofs   : 6 coefficients (a..f) per group (G*6)
 *  xv     : x argument per group (G)
 *  minv, maxv : clamp bounds per group (G)
 *  gid    : age -> group (0-based, length A)
 *  kk     : per-age scaling factor (A)
 *  byage  : 1 to also return per-age hourly totals (A x H)
 */
SEXP emis_speed_engine(SEXP speed, SEXP veh, SEXP lkm, SEXP profile,
                       SEXP code, SEXP clen, SEXP consts, SEXP soff,
                       SEXP cofs, SEXP xv, SEXP minv, SEXP maxv,
                       SEXP gid, SEXP kk,
                       SEXP Sid, SEXP Aid, SEXP Gid, SEXP Hid,
                       SEXP ntid, SEXP byageid) {
  int S = INTEGER(Sid)[0];
  int A = INTEGER(Aid)[0];
  int G = INTEGER(Gid)[0];
  int H = INTEGER(Hid)[0];
  int nt = INTEGER(ntid)[0];
  int byage = INTEGER(byageid)[0];

  const double *sp = REAL(speed);
  const double *vp = REAL(veh);
  const double *lp = REAL(lkm);
  const double *pp = REAL(profile);
  const int    *cd = INTEGER(code);
  const int    *cl = INTEGER(clen);
  const double *cs = REAL(consts);
  const int    *so = INTEGER(soff);
  const double *cf = REAL(cofs);
  const double *xp = REAL(xv);
  const double *mn = REAL(minv);
  const double *mx = REAL(maxv);
  const int    *gidx = INTEGER(gid);
  const double *kage = REAL(kk);

  /* code offset per group (0-based) */
  int *coff = (int *) R_alloc(G > 0 ? G : 1, sizeof(int));
  int acc = 0;
  for (int g = 0; g < G; g++) { coff[g] = acc; acc += cl[g]; }

  SEXP out_streets = PROTECT(allocVector(REALSXP, (R_xlen_t) S * H));
  SEXP out_veh = PROTECT(allocMatrix(REALSXP, A, H));
  double *os = REAL(out_streets);
  double *ov = REAL(out_veh);

  long long SH = (long long) S * H;
  for (long long i = 0; i < SH; i++) os[i] = 0.0;
  for (long long i = 0; i < (long long) A * H; i++) ov[i] = 0.0;

#ifdef _OPENMP
#pragma omp parallel num_threads(nt)
#endif
  {
    double *shp = (double *) malloc((size_t) (G > 0 ? G : 1) * sizeof(double));
    double *vsum = (double *) malloc((size_t) (A > 0 ? A : 1) * sizeof(double));
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int h = 0; h < H; h++) {
      double ph = pp[h];
      if (byage) for (int a = 0; a < A; a++) vsum[a] = 0.0;
      for (int i = 0; i < S; i++) {
        double spd = sp[(long long) i + (long long) S * h];
        double li = lp[i];
        for (int g = 0; g < G; g++) {
          double v = spd;
          if (v < mn[g]) v = mn[g];
          else if (v > mx[g]) v = mx[g];
          shp[g] = eval_rpn(cd + coff[g], cl[g], cs + so[g], v,
                            cf + (size_t) g * 6, xp[g]);
        }
        double sacc = 0.0;
        for (int a = 0; a < A; a++) {
          double contrib = vp[(long long) i + (long long) S * a] *
                           kage[a] * shp[gidx[a]] * li;
          sacc += contrib;
          if (byage) vsum[a] += contrib;
        }
        os[(long long) i + (long long) S * h] = ph * sacc;
      }
      if (byage) {
        for (int a = 0; a < A; a++)
          ov[(long long) a + (long long) A * h] = ph * vsum[a];
      }
    }
    free(shp);
    free(vsum);
  }

  SEXP out = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, out_streets);
  SET_VECTOR_ELT(out, 1, out_veh);
  SEXP nm = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(nm, 0, mkChar("streets"));
  SET_STRING_ELT(nm, 1, mkChar("veh"));
  setAttrib(out, R_NamesSymbol, nm);

  UNPROTECT(4);
  return out;
}
