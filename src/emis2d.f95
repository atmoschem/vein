SUBROUTINE emis2df (nrowv, ncolv, veh, lkm, ef, emis)

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: emis(nrowv, ncolv)

INTEGER i, j

DO i = 1, nrowv
   DO j = 1, ncolv
       emis(i, j) = veh(i,j) * lkm(i) * ef(j)
   ENDDO
ENDDO

RETURN
END
