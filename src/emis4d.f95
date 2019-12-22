SUBROUTINE emis4df ( nrowv, ncolv, proh, prod, veh, lkm, ef, pro, emis )

IMPLICIT none

INTEGER nrowv
INTEGER ncolv
INTEGER proh
INTEGER prod
real(kind = 8) :: veh(nrowv,ncolv)
real(kind = 8) :: lkm(nrowv)
real(kind = 8) :: ef(ncolv)
real(kind = 8) :: pro(proh,prod)
real(kind = 8) :: emis(nrowv,ncolv,proh, prod)

integer i, j, k, l
do i = 1, nrowv
   do j = 1, ncolv
      do k = 1, proh
         do l = 1, prod
           emis(i, j, k, l) = veh(i,j) * lkm(i) * ef(j)*pro(k,l)
         end do
      end do
   end do
end do

RETURN
END
