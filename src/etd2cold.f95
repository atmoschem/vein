SUBROUTINE emistd2coldf (nrowv, ncolv, veh, lkm, ef, efcold, beta,  emis) ! # nocov start

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv)
DOUBLE PRECISION :: beta(nrowv)
DOUBLE PRECISION :: emis(nrowv, ncolv)

INTEGER i, j

DO i = 1, nrowv
   DO j = 1, ncolv
      emis(i, j) = beta(i) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j)
   ENDDO
ENDDO

RETURN
END ! # nocov end
