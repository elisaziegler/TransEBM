! This subroutine calculates albedo values based on land masks and Legendre polynomials
! See reference:
! Gerald R. North and James A. Coakley, Jr., 1979,
! Differences between Seasonal and Mean Annual Energy Balance Model Calculations
! of Climate and Climate Sensistivity.
! Journal of the Atmospheric Sciences
! Vol 36, pp 1189-1204.
! 

subroutine extract(geography, filename, orig, a_land, a_land_lat, a_seaice, a_landice, a_ocean, a_ocean_lat)
    implicit none

    integer,parameter:: nx=128, ny=65
    real:: albedo(nx,ny), legendre
    integer:: i, j, alb
    integer:: geography(nx,ny)
    character(len=*) :: filename
    real :: a_land, a_land_lat, a_seaice, a_landice, a_ocean, a_ocean_lat
    logical :: orig

    if (orig) then
        a_land = 0.30 
        a_land_lat = 0.09 
        a_seaice = 0.60 
        a_landice = 0.70 !0.68 
        a_ocean = 0.29 
        a_ocean_lat = 0.09
    endif

    ! ================  newly added ==================================
    do j=1,ny
    legendre=0.5*(3*sin((90.0-(j-1)*2.8125)*3.1415926/180.0)*sin((90.0-(j-1)*2.8125)*3.1415926/180.0)-1)
        do i=1,nx
        alb=geography(i,j)
        if(alb.eq.1)  albedo(i,j)= a_land + a_land_lat*legendre
        if(alb.eq.2)  albedo(i,j)= a_seaice
        if(alb.eq.3)  albedo(i,j)= a_landice
        if(alb.eq.5)  albedo(i,j)= a_ocean + a_ocean_lat*legendre
        end do
    end do

    open(22,file=filename)
!    do j=ny, 1, -1
    do j=1, ny
        write(22,101) (albedo(i,j),i=1,nx)
        101 format(128F10.2)
    end do

    close(22)
end subroutine

