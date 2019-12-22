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

static const R_FortranMethodDef FortranEntries[] = {
  {"emis2df", (DL_FUNC) &F77_NAME(emis2df), 6},
  {"emis3df", (DL_FUNC) &F77_NAME(emis3df), 8},
  {"emis4df", (DL_FUNC) &F77_NAME(emis4df), 9},
  {NULL, NULL, 0}
};



void R_init_vein(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
