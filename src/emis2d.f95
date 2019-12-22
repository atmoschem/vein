SUBROUTINE emis2df ( nrowv, ncolv, veh, lkm, ef,  emis )
IMPLICIT none

INTEGER nrowv
INTEGER ncolv
real(kind = 8) :: veh(nrowv,ncolv)
real(kind = 8) :: lkm(nrowv)
real(kind = 8) :: ef(ncolv)
real(kind = 8) :: emis(nrowv,ncolv)

integer i, j
do i = 1, nrowv
   do j = 1, ncolv
       emis(i, j) = veh(i,j) * lkm(i) * ef(j)
   end do
end do

RETURN
END
