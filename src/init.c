#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Fortran calls */
extern void F77_NAME(emis2df)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emis3df)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emis4df)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd1f)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd2coldf)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd2f)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd3coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd3f)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd4coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd4f)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd5coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd5f)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd6coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd6f)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd7f)(void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
  {"emis2df",      (DL_FUNC) &F77_NAME(emis2df),       6},
  {"emis3df",      (DL_FUNC) &F77_NAME(emis3df),       8},
  {"emis4df",      (DL_FUNC) &F77_NAME(emis4df),       9},
  {"emistd1f",     (DL_FUNC) &F77_NAME(emistd1f),      8},
  {"emistd2coldf", (DL_FUNC) &F77_NAME(emistd2coldf), 8},
  {"emistd2f",     (DL_FUNC) &F77_NAME(emistd2f),      8},
  {"emistd3coldf", (DL_FUNC) &F77_NAME(emistd3coldf), 10},
  {"emistd3f",     (DL_FUNC) &F77_NAME(emistd3f),      8},
  {"emistd4coldf", (DL_FUNC) &F77_NAME(emistd4coldf), 10},
  {"emistd4f",     (DL_FUNC) &F77_NAME(emistd4f),      8},
  {"emistd5coldf", (DL_FUNC) &F77_NAME(emistd5coldf), 10},
  {"emistd5f",     (DL_FUNC) &F77_NAME(emistd5f),      8},
  {"emistd6coldf", (DL_FUNC) &F77_NAME(emistd6coldf), 10},
  {"emistd6f",     (DL_FUNC) &F77_NAME(emistd6f),      8},
  {"emistd7f",     (DL_FUNC) &F77_NAME(emistd7f),      6},
  {NULL, NULL, 0}
};

void R_init_vein(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
