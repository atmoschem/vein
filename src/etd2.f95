SUBROUTINE emistd2f (nrowv, ncolv, pmonth, veh, lkm, ef, month, emis) ! # nocov start

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
