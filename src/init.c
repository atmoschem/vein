#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
s quiere decir que tiene speed y factores de emissioens cambian horariamente
 */

/* .Fortran calls */
extern void F77_NAME(emis2df)(void *nrowv, void *ncolv, void *veh, void *lkm, void *ef, void *emis);
extern void F77_NAME(emis3df)(void *nrowv, void *ncolv, void *prok, void *veh, void *lkm, void *ef, void *pro, void *emis);
extern void F77_NAME(emis4df)(void *nrowv, void *ncolv, void *proh, void *prod, void *veh, void *lkm, void *ef, void *pro, void *emis);
extern void F77_NAME(emistd1f)(void *nrowv, void *ncolv, void *pmonth, void *veh, void *lkm, void *ef, void *month, void *emis);
extern void F77_NAME(emistd2f)(void *nrowv, void *ncolv, void *pmonth, void *veh, void *lkm, void *ef, void *month, void *emis);
extern void F77_NAME(emistd3f)(void *nrowv, void *ncolv, void *pmonth, void *nrowvp, void *veh, void *lkm, void *ef, void *month, void *emis);
extern void F77_NAME(emistd4f)(void *nrowv, void *ncolv, void *pmonth, void *nrowvp, void *veh, void *lkm, void *ef, void *month, void *emis);
extern void F77_NAME(emistd5f)(void *nrowv, void *ncolv, void *pmonth, void *veh, void *lkm, void *ef, void *month, void *emis);
extern void F77_NAME(emistd6f)(void *nrowv, void *ncolv, void *pmonth, void *veh, void *lkm, void *ef, void *month, void *emis);

static const R_FortranMethodDef FortranEntries[] = {
  {"emis2df", (DL_FUNC) &F77_NAME(emis2df), 6},
  {"emis3df", (DL_FUNC) &F77_NAME(emis3df), 8},
  {"emis4df", (DL_FUNC) &F77_NAME(emis4df), 9},
  {"emistd1f", (DL_FUNC) &F77_NAME(emistd1f), 8},
  {"emistd2f", (DL_FUNC) &F77_NAME(emistd2f), 8},
  {"emistd3f", (DL_FUNC) &F77_NAME(emistd3f), 9},
  {"emistd4f", (DL_FUNC) &F77_NAME(emistd4f), 9},
  {"emistd5f", (DL_FUNC) &F77_NAME(emistd5f), 8},
  {"emistd6f", (DL_FUNC) &F77_NAME(emistd6f), 8},
  {NULL, NULL, 0}
};



void R_init_vein(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
