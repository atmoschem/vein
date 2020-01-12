SUBROUTINE emis3df ( nrowv, ncolv, prok, veh, lkm, ef, pro, emis )

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER prok
DOUBLE PRECISION :: veh(nrowv,ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: pro(prok)
DOUBLE PRECISION :: emis(nrowv,ncolv,prok)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, prok
        emis(i, j,k) = veh(i,j) * lkm(i) * ef(j)*pro(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END
