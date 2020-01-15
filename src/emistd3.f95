SUBROUTINE emistd3f (nrowv, ncolv, pmonth, nrowvp, veh, lkm, ef, month, emis)

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
INTEGER nrowvp
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(nrowvp, ncolv)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j, k) = veh(i, j) * lkm(i) * ef(i*k, j) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END
