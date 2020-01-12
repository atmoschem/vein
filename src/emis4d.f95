SUBROUTINE emis4df ( nrowv, ncolv, proh, prod, veh, lkm, ef, pro, emis )

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER proh
INTEGER prod
DOUBLE PRECISION :: veh(nrowv,ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: pro(proh,prod)
DOUBLE PRECISION :: emis(nrowv,ncolv,proh, prod)

INTEGER i, j, k, l

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, proh
         DO l = 1, prod
           emis(i, j, k, l) = veh(i,j) * lkm(i) * ef(j)*pro(k,l)
         ENDDO
      ENDDO
   ENDDO
ENDDO

RETURN
END
