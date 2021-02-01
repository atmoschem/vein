#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Fortran calls */
extern void F77_NAME(checkntf)(void *);
extern void F77_NAME(emis2df)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emis3df)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emis4df)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd1f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd1fpar)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd2coldf)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd2f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd2fpar)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd3coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd3f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd3fpar)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd4coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd4f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd4fpar)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd5coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd5f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd5fpar)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd6coldf)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd6f)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd6fpar)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd7f)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(emistd7fpar)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
  {"checkntf",     (DL_FUNC) &F77_NAME(checkntf),      1},
  {"emis2df",      (DL_FUNC) &F77_NAME(emis2df),       6},
  {"emis3df",      (DL_FUNC) &F77_NAME(emis3df),       8},
  {"emis4df",      (DL_FUNC) &F77_NAME(emis4df),       9},
  {"emistd1f",     (DL_FUNC) &F77_NAME(emistd1f),     11},
  {"emistd1fpar",  (DL_FUNC) &F77_NAME(emistd1fpar),  12},
  {"emistd2coldf", (DL_FUNC) &F77_NAME(emistd2coldf),  8},
  {"emistd2f",     (DL_FUNC) &F77_NAME(emistd2f),     11},
  {"emistd2fpar",  (DL_FUNC) &F77_NAME(emistd2fpar),  12},
  {"emistd3coldf", (DL_FUNC) &F77_NAME(emistd3coldf), 10},
  {"emistd3f",     (DL_FUNC) &F77_NAME(emistd3f),     11},
  {"emistd3fpar",  (DL_FUNC) &F77_NAME(emistd3fpar),  12},
  {"emistd4coldf", (DL_FUNC) &F77_NAME(emistd4coldf), 10},
  {"emistd4f",     (DL_FUNC) &F77_NAME(emistd4f),     13},
  {"emistd4fpar",  (DL_FUNC) &F77_NAME(emistd4fpar),  13},
  {"emistd5coldf", (DL_FUNC) &F77_NAME(emistd5coldf), 10},
  {"emistd5f",     (DL_FUNC) &F77_NAME(emistd5f),     11},
  {"emistd5fpar",  (DL_FUNC) &F77_NAME(emistd5fpar),  12},
  {"emistd6coldf", (DL_FUNC) &F77_NAME(emistd6coldf), 10},
  {"emistd6f",     (DL_FUNC) &F77_NAME(emistd6f),     11},
  {"emistd6fpar",  (DL_FUNC) &F77_NAME(emistd6fpar),  12},
  {"emistd7f",     (DL_FUNC) &F77_NAME(emistd7f),      9},
  {"emistd7fpar",  (DL_FUNC) &F77_NAME(emistd7fpar),  10},
  {NULL, NULL, 0}
};

void R_init_vein(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
