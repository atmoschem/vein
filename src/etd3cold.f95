SUBROUTINE emistd3coldf (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, emis)

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: beta(nrowv, pmonth)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j, k) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END
