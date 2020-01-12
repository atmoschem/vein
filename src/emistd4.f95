SUBROUTINE emistd4f (nrowv, ncolv, pmonth, nrowvp, veh, lkm, ef, month, emis)

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
INTEGER nrowvp
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(nrowvp, ncolv)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth, nrowvp)

INTEGER i, j, k, l

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        DO l = 1, nrowvp
          emis(i, j, k, l) = veh(i, j) * lkm(i) * ef(l, j) * month(i,k)
        ENDDO
      ENDDO
   ENDDO
ENDDO

RETURN
END
