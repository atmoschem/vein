!------------------------------------------------------------------
! Fortran subroutines for top-down approach
! Todo: Improve documentation

SUBROUTINE emistd1f (nrowv, ncolv, pmonth, veh, lkm, ef, month, emis) ! # nocov start

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j,k) = veh(i,j) * lkm(j) * ef(i,j)*month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd2f (nrowv, ncolv, pmonth, veh, lkm, ef, month, emis) ! # nocov startz
IMPLICIT NONE

! subroutine to estimate emissions in emis_hot_td
! in R:
! nrowv  = as.integer(nrow(veh))
! ncolv  = as.integer(ncol(veh))
! pmonth = as.integer(ncol(pro_month))
! lkm    = as.numeric(lkm)
! ef     = as.matrix(ef[, 1:ncol(veh)])
! month  = as.matrix(pro_month)



INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv,ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv,ncolv)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv,ncolv,pmonth)

INTEGER i, j, k


DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j)*month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end



!------------------------------------------------------------------
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


!------------------------------------------------------------------
SUBROUTINE emistd3f (nrowv, ncolv, pmonth, veh, lkm, ef, month, emis) ! # nocov start

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j, k) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd3coldf (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, emis) ! # nocov start

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
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd4f (nrowv, ncolv, pmonth, veh, lkm, ef, month, emis) ! # nocov start

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j, k) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
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

!------------------------------------------------------------------
SUBROUTINE emistd5f (nrowv, ncolv, pmonth, veh, lkm, ef, month, emis) ! # nocov start

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j,k) = veh(i,j) * lkm(j) * ef(j) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end


!------------------------------------------------------------------
SUBROUTINE emistd5coldf (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, emis) ! # nocov start

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: beta(nrowv, pmonth)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(i, j) * efcold(i, j, k) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd6f (nrowv, ncolv, pmonth, veh, lkm, ef, month, emis) ! # nocov start

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: month(nrowv,pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j, k) = veh(i,j) * lkm(j) * ef(j) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd6coldf (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, emis) ! # nocov start

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: beta(nrowv, pmonth)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j, k) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd7f (nrowv, ncolv, veh, lkm, ef, emis) ! # nocov start

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: emis(nrowv, ncolv)

INTEGER i, j

DO i = 1, nrowv
   DO j = 1, ncolv
       emis(i, j) = veh(i,j) * lkm(j) * ef(j)
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd1fpar (nrowv, ncolv, pmonth, veh, lkm, ef, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT none
INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k, nt

!$CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j,k) = veh(i,j) * lkm(j) * ef(i,j)*month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd2fpar (nrowv, ncolv, pmonth, veh, lkm, ef, month, nt, emis) ! # nocov startz
!$ USE OMP_LIB

IMPLICIT NONE

! subroutine to estimate emissions in emis_hot_td
! in R:
! nrowv  = as.integer(nrow(veh))
! ncolv  = as.integer(ncol(veh))
! pmonth = as.integer(ncol(pro_month))
! lkm    = as.numeric(lkm)
! ef     = as.matrix(ef[, 1:ncol(veh)])
! month  = as.matrix(pro_month)

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv,ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv,ncolv)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv,ncolv,pmonth)

INTEGER i, j, k, nt

!$CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j)*month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd2coldfpar (nrowv, ncolv, veh, lkm, ef, efcold, beta,  nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv)
DOUBLE PRECISION :: beta(nrowv)
DOUBLE PRECISION :: emis(nrowv, ncolv)

INTEGER i, j, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      emis(i, j) = beta(i) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j)
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd3fpar (nrowv, ncolv, pmonth, veh, lkm, ef, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j, k) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd3coldfpar (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

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

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)

emis = 0.0

!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)


DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j, k) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd4fpar (nrowv, ncolv, pmonth, veh, lkm, ef, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)
emis = 0.0
!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j, k) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd4coldfpar (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

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

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)
emis = 0.0
!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(i, j) * efcold(i, j, k) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd5fpar (nrowv, ncolv, pmonth, veh, lkm, ef, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)
emis = 0.0
!$OMP PARALLEL DO PRIVATE(i, j) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j,k) = veh(i,j) * lkm(j) * ef(j) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd5coldfpar (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(nrowv, ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: beta(nrowv, pmonth)
DOUBLE PRECISION :: month(pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)
emis = 0.0
!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(i, j) * efcold(i, j, k) * month(k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd6fpar (nrowv, ncolv, pmonth, veh, lkm, ef, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: month(nrowv,pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)
emis = 0.0
!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
        emis(i, j, k) = veh(i,j) * lkm(j) * ef(j) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd6coldfpar (nrowv, ncolv, pmonth, veh, lkm, ef, efcold, beta, month, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT NONE

INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: efcold(nrowv, ncolv, pmonth)
DOUBLE PRECISION :: beta(nrowv, pmonth)
DOUBLE PRECISION :: month(nrowv, pmonth)
DOUBLE PRECISION :: emis(nrowv, ncolv, pmonth)

INTEGER i, j, k, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)
emis = 0.0
!$OMP PARALLEL DO PRIVATE(i, j, k) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
      DO k = 1, pmonth
         emis(i, j, k) = beta(i, k) * veh(i, j) * lkm(j) * ef(j) * efcold(i, j, k) * month(i, k)
      ENDDO
   ENDDO
ENDDO

RETURN
END ! # nocov end

!------------------------------------------------------------------
SUBROUTINE emistd7fpar (nrowv, ncolv, veh, lkm, ef, nt, emis) ! # nocov start
!$ USE OMP_LIB

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
DOUBLE PRECISION :: veh(nrowv, ncolv)
DOUBLE PRECISION :: lkm(ncolv)
DOUBLE PRECISION :: ef(ncolv)
DOUBLE PRECISION :: emis(nrowv, ncolv)

INTEGER i, j, nt

!$ CALL OMP_SET_DYNAMIC(.TRUE.)
emis = 0.0
!$OMP PARALLEL DO PRIVATE(i, j) DEFAULT(shared) NUM_THREADS(nt)

DO i = 1, nrowv
   DO j = 1, ncolv
       emis(i, j) = veh(i,j) * lkm(j) * ef(j)
   ENDDO
ENDDO

RETURN
END ! # nocov end

