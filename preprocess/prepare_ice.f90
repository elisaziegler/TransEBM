! THIS DOESN'T DO ANYTHING YET!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
!also can no longer be used like this due to parallelizing (config, albedo.dat)
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine prepare_ice
use configuration_parser
implicit none

integer,parameter:: nx=128, ny=65
real:: albedo(nx,ny), legendre
integer:: i, j, alb
integer:: geography(nx,ny)

!     Configuration file parsing
type(configuration) :: conf

call conf%parse_config('../config/config.conf')

open(20, file = conf%world, status = 'old') !has to be changed in extract.f90, too!!!
!open(20,file='The_World.dat')

  do j = ny, 1, -1
    read (20,100) (geography(i,j),i=1,nx)
100  format (128I1)
  end do

! ================  newly added ==================================
do j=1,ny
   legendre=0.5*(3*sin((90.0-(j-1)*2.8125)*3.1415926/180.0)*sin((90.0-(j-1)*2.8125)*3.1415926/180.0)-1)
do i=1,nx
   alb=geography(i,j)
  if(alb.eq.1)  albedo(i,j)=0.30 +0.09*legendre
  if(alb.eq.2)  albedo(i,j)=0.60
  if(alb.eq.3)  albedo(i,j)=0.70 !0.68 
  if(alb.eq.5)  albedo(i,j)=0.29 +0.09*legendre
end do
end do

open(21,file='albedo.dat')
do j=ny, 1, -1
  write(21,101) (albedo(i,j),i=1,nx)
101 format(128F10.2)
end do

close(21)
close(20)
end subroutine
