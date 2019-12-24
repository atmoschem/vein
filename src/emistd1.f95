SUBROUTINE emistd1f ( nrowv, ncolv, pmonth, veh, lkm, ef, month, emis )
IMPLICIT none
INTEGER nrowv
INTEGER ncolv
INTEGER pmonth
real(kind = 8) :: veh(nrowv,ncolv)
real(kind = 8) :: lkm(nrowv)
real(kind = 8) :: ef(nrowv,ncolv)
real(kind = 8) :: month(nrowv,pmonth)
real(kind = 8) :: emis(nrowv,ncolv,pmonth)

integer i, j, k
do i = 1, nrowv
   do j = 1, ncolv
      do k = 1, pmonth
        emis(i, j,k) = veh(i,j) * lkm(i) * ef(i,j)*month(i,k)
      end do
   end do
end do

RETURN
END
