SUBROUTINE emis3df ( nrowv, ncolv, prok, veh, lkm, ef, pro, emis )
IMPLICIT none
INTEGER nrowv
INTEGER ncolv
INTEGER prok
real(kind = 8) :: veh(nrowv,ncolv)
real(kind = 8) :: lkm(nrowv)
real(kind = 8) :: ef(ncolv)
real(kind = 8) :: pro(prok)
real(kind = 8) :: emis(nrowv,ncolv,prok)

integer i, j, k
do i = 1, nrowv
   do j = 1, ncolv
      do k = 1, prok
        emis(i, j,k) = veh(i,j) * lkm(i) * ef(j)*pro(k)
      end do
   end do
end do

RETURN
END
