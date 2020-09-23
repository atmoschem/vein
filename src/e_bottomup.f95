SUBROUTINE emis2df (nrowv, ncolv, veh, lkm, ef, emis) ! # nocov start

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
END  ! # nocov end

!-------------------------------------------------------------------
! parallel starts

SUBROUTINE emis2dfpar (nrowv, ncolv, veh, lkm, ef, emis, nt) ! # nocov start
USE OMP_LIB
IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: emis(nrowv, ncolv)

INTEGER i, j, nt


CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j) DEFAULT(shared) NUM_THREADS(nt)
DO i = 1, nrowv
   DO j = 1, ncolv
       emis(i, j) = veh(i,j) * lkm(i) * ef(j)
   ENDDO
ENDDO

RETURN
END  ! # nocov end

SUBROUTINE emis3df ( nrowv, ncolv, prok, veh, lkm, ef, pro, emis ) ! # nocov start

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
END ! # nocov end


!-------------------------------------------------------------------
! parallel starts

SUBROUTINE emis3dfpar ( nrowv, ncolv, prok, veh, lkm, ef, pro, emis, nt) ! # nocov start
USE OMP_LIB
IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER prok
DOUBLE PRECISION :: veh(nrowv,ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: pro(prok)
DOUBLE PRECISION :: emis(nrowv,ncolv,prok)

INTEGER i, j, k, nt


CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)
DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, prok
        emis(i, j, k) = veh(i, j) * lkm(i) * ef(j)*pro(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

SUBROUTINE emis4df ( nrowv, ncolv, proh, prod, veh, lkm, ef, pro, emis ) ! # nocov start

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
END ! # nocov end

!-------------------------------------------------------------------
! parallel starts

SUBROUTINE emis4dfpar (nrowv, ncolv, proh, prod, veh, lkm, ef, pro, emis, nt) ! # nocov start
USE OMP_LIB
IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER proh
INTEGER prod
DOUBLE PRECISION :: veh(nrowv,ncolv)
DOUBLE PRECISION :: lkm(nrowv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: pro(proh,prod)
DOUBLE PRECISION :: emis(nrowv,ncolv,proh, prod)

INTEGER i, j, k, l, nt


CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j, k, l) DEFAULT(shared) NUM_THREADS(nt)
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
END ! # nocov end

