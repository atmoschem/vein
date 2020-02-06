SUBROUTINE emistd4coldf (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, emis) ! # nocov start

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: beta(nrowv, pmonth)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(i, j) * efcold(i, j, k) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end
