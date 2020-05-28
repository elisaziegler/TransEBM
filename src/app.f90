! Here list necessary subroutines to calculate
! global.f90
!*********************************************************************

REAL FUNCTION Global (u, area, hem)

IMPLICIT NONE
INCLUDE 'ebm.inc'

real::  u(NX6,NY6)
real::  area(NY6)
real::  sum, asum
integer:: i, j, hem
!---------------------------------------------------------------------
! Compute global mean of array

      if (hem .eq. 0) then                       ! global
        sum = 0.0
        do j = 2, NY6-1                          ! non-pole points
          do i = 1, NX6
            sum = sum + area(j)*u(i,j)
          end do
        end do
        sum = sum + area(1)*(u(1,1) + u(1,NY6))   ! pole points
        Global = sum 
      else if (hem .eq. 1) then                   ! northern hemisphere
        sum = 0.0
        asum = 0.0
        do j = 2, 32 
          do i = 1, NX6
            sum = sum + area(j)*u(i,j)
            asum = asum + area(j)
          end do
        end do
        sum = sum + area(1)*u(1,1)
        asum = asum + area(1)
        Global = sum/asum 
      else if (hem .eq. 2) then                   ! southern hemisphere
        sum = 0.0
        asum = 0.0
        do j = 34, NY6-1 
          do i = 1, NX6
            sum = sum + area(j)*u(i,j)
            asum = asum + area(j)
          end do
        end do
        sum = sum + area(NY6)*u(1,NY6)
        asum = asum + area(NY6)
        Global = sum/asum 
      end if

END FUNCTION

! heatcapacities.f90
!*************************************************************************
! Read in the map of the geography of the earth. Assign the proper heat
! capacities at each grid point. Calculate the zero order term for the
! LHS for musd2, which depends on the heat capacity.
!---------------------------------------------------------------------------

SUBROUTINE HeatCapacities (heatcap, geography, tau_land, tau_snow, &
                           tau_sea_ice, tau_mixed_layer, orig, C_atmos, &
                           C_soil, C_seaice, C_snow, C_mixed_layer)

IMPLICIT NONE
INCLUDE 'ebm.inc'

integer,parameter:: nx = NX6, ny = NY6
real:: heatcap(nx,ny)
real:: tau_land                ! thermal relaxation times
real:: tau_sea_ice
real:: tau_snow
real:: tau_mixed_layer
real:: C_seaice                ! heat capacities
real:: C_mixed_layer
real:: C_soil
real:: C_snow
real:: C_atmos
real::  sum, z
integer:: i, j, n, geo
integer:: geography(nx,ny)
logical:: orig

! Depths 
real,parameter:: depth_atmos = 5000.               ! meters
real,parameter:: depth_mixed_layer = 70.0          ! meters
real,parameter:: depth_soil = 2.0                  ! meters
real,parameter:: depth_seaice = 2.5                ! meters
real,parameter:: depth_snow = 2.0                  ! meters
real,parameter:: layer_depth = 0.5                 ! kilometers

! Physical properties of atmosphere
real,parameter:: rho_atmos = 1.293           ! kg m^-3  dry air (STP)
real,parameter:: csp_atmos = 1005.0          ! J kg^-1 K^-1 (STP)
real,parameter:: scale_height = 7.6          ! kilometers
        
! Physical properties of water
real,parameter:: rho_water = 1000.0          ! kg m^-3
real,parameter:: csp_water = 4186.0          ! J kg^-1 K^-1

! Physical properties of soil 
real,parameter:: rho_soil = 1100.0           ! kg m^-3   
real,parameter:: csp_soil = 850.0            ! J kg^-1 K^-1
      
! Physical properties of sea ice
real,parameter:: rho_sea_ice = 917.0         ! kg m^-3
real,parameter:: csp_sea_ice = 2106.0        ! J kg^-1 K^-1  

! Physical properties of snow covered surface
real,parameter:: rho_snow = 400.0            ! kg m^-3
real,parameter:: csp_snow = 1900.0           ! J kg^-1 K^-1
       
! Other constants  
real,parameter:: sec_per_yr = 3.15576e7      ! seconds per year
real,parameter:: days_per_yr = 365.2422      ! days per year

! Calculate Heat capacities for constituents

! atmosphere with exponentially decaying density
  sum = 0.0
  do n = 1, 10          
    z = (0.25 + layer_depth*real(n-1))/scale_height
    sum = sum + exp(-z)
  end do

!  write(*,*) 'sec_per_yr=', sec_per_yr, ', sum=', sum
 
 if (orig) then 
    C_atmos = csp_atmos*layer_depth*1000.0*rho_atmos*sum/sec_per_yr
    C_soil = depth_soil*rho_soil*csp_soil/sec_per_yr 
    C_seaice = depth_seaice*rho_sea_ice*csp_sea_ice/sec_per_yr
    C_snow = depth_snow * rho_snow * csp_snow/sec_per_yr
    C_mixed_layer = depth_mixed_layer*rho_water*csp_water/sec_per_yr
!    write(*,*) 'c_atmos=', c_atmos, ', c_soil=', c_soil, 'c_seaice=', c_seaice, &
!        ', c_snow', c_snow, ', c_mixed=', C_mixed_layer
 endif

! Calculate radiative relaxation times for columns
 tau_land = (C_soil + C_atmos)/B * days_per_yr    
 tau_snow = (C_snow + C_atmos)/B * days_per_yr 
 tau_sea_ice = (C_seaice + C_atmos)/B * days_per_yr  
 tau_mixed_layer = (C_mixed_layer + C_atmos)/B     
           
! Assign the correct value of the heat capacity of the columns
 do j = 1, ny
   do i = 1, nx
     geo  = geography(i,j)
     if (geo == 1) then                        ! land
       heatcap(i,j) = C_soil + C_atmos   
     else if (geo==2) then                     !  perennial sea ice
       heatcap(i,j) = C_seaice + C_atmos
     else if (geo == 3) then                   ! permanent snow cover 
       heatcap(i,j) = C_snow + C_atmos         
     else if (geo == 4) then                   ! lakes, inland seas
       heatcap(i,j) = C_mixed_layer/3.0 + C_atmos 
     else if (geo == 5) then                   ! Pacific ocean 
       heatcap(i,j) = C_mixed_layer + C_atmos
     else if (geo == 6) then                   ! Atlantic ocean 
       heatcap(i,j) = C_mixed_layer + C_atmos
     else if (geo == 7) then                   ! Indian ocean 
       heatcap(i,j) = C_mixed_layer + C_atmos
     else if (geo == 8) then                   ! Mediterranean 
       heatcap(i,j) = C_mixed_layer + C_atmos
     end if                           
   end do
 end do  

! write(*,*) 'c_land = ', (C_soil + C_atmos)
! write(*,*) 'c_seaice = ', (C_seaice + C_atmos)
! write(*,*) 'c_snow = ', (C_snow + C_atmos)
! write(*,*) 'c_ocean =', (C_mixed_layer + C_atmos)

END SUBROUTINE

! inittemp.f90
!**********************************************************************
! You can read into your own initial temperature here
SUBROUTINE Initial_Temp (Temp)

IMPLICIT NONE
INCLUDE 'ebm.inc'

integer i,j
real:: Temp(NX6,NY6)
real,allocatable:: data(:,:)

allocate (data(NX6,NY6))

do i=1,nx6
do j=1,ny6
temp(i,j)=8
end do
end do

deallocate (data)

END SUBROUTINE

! solar_forcing
!***************************************************************************
! Calculates the solar forcing for the current time step (ts)
! @param Pcoalbedo: planetary co-albedo field at current time step
! @param A: parameter depending on current CO2
! @param solar: TOA insolation for current simulation year
! @param SF: solar forcing field for current time step, output
! @param ts: current time step
!---------------------------------------------------------------------------

SUBROUTINE Solar_Forcing (Pcoalbedo, A, solar, SF, ts)
IMPLICIT NONE
INCLUDE 'ebm.inc'

integer:: i, j, ts
real:: A
real,dimension(NX6,NY6):: Pcoalbedo, SF
real:: solar(NY6,NT)
!-------------------------------------------------------------------------

! calcullate the solar forcing
do j = 1, NY6
  do i = 1, NX6
    SF(i,j) = solar(j,ts)*Pcoalbedo(i,j) - A
  end do
end do

END SUBROUTINE

! sinCosTanLat
!***************************************************************************
! Returns the sine, cosine and tangens of all latitudes
! @param siny,cosy,tany: latidunial field of the sine/ cosine/ tangens
!---------------------------------------------------------------------------

subroutine sinCosTanLat (siny, cosy, tany)
implicit none
include 'ebm.inc'

integer :: j
real :: lat, siny(NY6), cosy(NY6), tany(NY6)
!-------------------------------------------------------------------------

! Calculate the sin, cos, and tan of the latitudes of Earth from the 
! colatitudes
  siny = 0.0
  cosy = 0.0
  tany = 0.0

  do j = 1, NY6 
    lat = pi/2.0 - dy*real(j-1)       ! latitude in radians
    siny(j) = sin(lat) 
    if (j==1) then
      cosy(j) = 0.0
      tany(j) = 1000.0
    else if (j==NY6) then
      cosy(j) = 0.0
      tany(j) = -1000.0 
    else
      cosy(j) = cos(lat)
      tany(j) = tan(lat) 
    end if
  end do

end subroutine sinCosTanLat


! calc_insolation
!***************************************************************************
! Returns the Top of Atmosphere insolation for any latitude of the 
! earth at the needed time steps. Calls the routine insolation, which does 
! the actual calculations.
! @param yr: current simulation year
! @param S0: solar constant
! @param ecc: eccentricity of the Earth's orbit
! @param ob: obliquity of the Earth's axis
! @param per: longitude of perihelion
!---------------------------------------------------------------------------

SUBROUTINE calc_insolation (S0, ecc, ob, per, solar, siny, cosy, tany)
IMPLICIT NONE
INCLUDE 'ebm.inc'

real:: ob, ecc, per, S0(NT)
real:: lambda(NT+1), solar(NY6,NT), siny(NY6), cosy(NY6), tany(NY6)
!-------------------------------------------------------------------------

! Calculate the sin, cos, and tan of the latitudes of Earth from the 
! colatitudes, calculate the insolation
 
call insolation (pi, dt, ob, ecc, per, NT, NY6, siny, cosy, tany, lambda, &
                S0, solar)  

END SUBROUTINE

!***************************************************************************

SUBROUTINE insolation (pi, dt, ob, ecc, per, nt, nlat, siny, cosy, tany, &
                       lambda, S0, solar) 
 
!  This subroutine computes the insolation at the top of the atmosphere as
!  a function of latitude and time. Reference: Andre Berger, JOURNAL OF THE
!  ATMOSPHERIC SCIENCES, Vol. 35, pgs. 2362-2367, December 1978.
 
! Inputs to the subroutine 
!     pi     = pi
!     dt     = time step increment
!     ob     = obliquity of the earth's axis
!     ecc    = eccentricity of the earth's orbit
!     per    = longitude of perihelion measured from the moving vernal equinox
!     nt+1   = number of time steps per year in the orbital calculation
!     nlat   = number of latitudes at which the insolation is computed
!     cosy   = cosines of the latitudes at which the insolation is computed 
!     siny   = sines of the latitudes at which the insolation is computed 
!     tany   = tangents of the latitudes at which the insolation is computed
!     lambda = true longitude of the earth counterclockwise from the vernal
!              equinox
!     S0     = solar constant for time n  
!
! Output from the subroutine
!     solar      = solar insolation values for latitude and time
!--------------------------------------------------------------------------- 
IMPLICIT NONE

! Input variables
integer:: nt, nlat
real,dimension(nlat):: siny, cosy, tany
real:: pi, dt, ob, ecc, per 
real:: lambda(nt+1), S0(nt)

! Output variables
real:: solar(nlat,nt)

! Local variables
integer:: j, n
real:: angle, cosdec, eccfac, rhofac, rzero, sindec 
real:: tandec, func, nu, Hzero, t1, t2, t3, t4, z

! Define the function for the Runge-Kutta method
func(angle) = rzero*(1.0 - ecc*COS(angle))**2
!---------------------------------------------------------------------------- 
 
eccfac = 1.0 - ecc**2
rzero  = (2.0*pi)/eccfac**1.5

!  Solve the orbital equation for lambda as a function of time with
!  a fourth-order Runge-Kutta method
lambda(1) = 0.0
DO n = 2, nt+1
  nu = lambda(n-1) - per
  t1 = dt*func(nu)
  t2 = dt*func(nu + 0.5*t1)
  t3 = dt*func(nu + 0.5*t2)
  t4 = dt*func(nu + t3)
  lambda(n) = lambda(n-1) + (t1 + 2.0*t2 + 2.0*t3 + t4)/6.0
END DO
 
!  Compute the average daily solar irradiance as a function of
!  latitude and longitude (time)
DO n = 1, nt
  nu = lambda(n) - per
  rhofac = ((1.0- ecc*COS(nu))/eccfac)**2 !actually minus b/c of the 180 degree offset to omega in Berger paper
  sindec = SIN(ob)*SIN(lambda(n))
  cosdec = SQRT(1.0-sindec**2)
  tandec = sindec/cosdec
  DO j = 1, nlat
    z = -tany(j)*tandec
    IF (z >= 1.0) THEN   ! polar latitudes when there is no sunrise (winter)
      solar(j,n) = 0.0                  
    ELSE          
      IF (z <= -1.0) THEN                ! when there is no sunset (summer)
        solar(j,n) = rhofac * S0(n) * siny(j) * sindec 
      ELSE
        Hzero = ACOS(z)
        solar(j,n) = rhofac/pi*S0(n)*(Hzero*siny(j)*sindec +  &
                                      cosy(j)*cosdec*SIN(Hzero))
      END IF
    END IF
  END DO
END DO
 
END SUBROUTINE

!***************************************************************************

SUBROUTINE insol (pi, dt, ob, ecc, per, nt, nlat, siny, cosy, tany, &
                       lambda, S0, solar) 
 
!  This subroutine computes the insolation at the top of the atmosphere as
!  a function of latitude and time. Reference: Andre Berger, JOURNAL OF THE
!  ATMOSPHERIC SCIENCES, Vol. 35, pgs. 2362-2367, December 1978.
 
! Inputs to the subroutine 
!     pi     = pi
!     dt     = time step increment
!     ob     = obliquity of the earth's axis
!     ecc    = eccentricity of the earth's orbit
!     per    = longitude of perihelion measured from the moving vernal equinox
!     nt+1   = number of time steps per year in the orbital calculation
!     nlat   = number of latitudes at which the insolation is computed
!     cosy   = cosines of the latitudes at which the insolation is computed 
!     siny   = sines of the latitudes at which the insolation is computed 
!     tany   = tangents of the latitudes at which the insolation is computed
!     lambda = true longitude of the earth counterclockwise from the vernal
!              equinox
!     S0     = solar constant for time n  
!
! Output from the subroutine
!     solar      = solar insolation values for latitude and time
!--------------------------------------------------------------------------- 
IMPLICIT NONE

! Input variables
integer:: nt, nlat
real,dimension(nlat):: siny, cosy, tany
real:: pi, dt, ob, ecc, per 
real:: lambda(nt+1), S0(nt)

! Output variables
real:: solar(nlat,nt)

! Local variables
integer:: j, n
real:: angle, cosdec, eccfac, rhofac, rzero, sindec 
real:: tandec, func, nu, Hzero, t1, t2, t3, t4, z

! Define the function for the Runge-Kutta method
func(angle) = rzero*(1.0 - ecc*COS(angle))**2
!---------------------------------------------------------------------------- 
 
eccfac = 1.0 - ecc**2
rzero  = (2.0*pi)/eccfac**1.5

!  Solve the orbital equation for lambda as a function of time with
!  a fourth-order Runge-Kutta method
lambda(1) = 0.0
DO n = 2, nt+1
  nu = lambda(n-1) - per
  t1 = dt*func(nu)
  t2 = dt*func(nu + 0.5*t1)
  t3 = dt*func(nu + 0.5*t2)
  t4 = dt*func(nu + t3)
  lambda(n) = lambda(n-1) + (t1 + 2.0*t2 + 2.0*t3 + t4)/6.0
END DO
 
!  Compute the average daily solar irradiance as a function of
!  latitude and longitude (time)
DO n = 1, nt
  nu = lambda(n) - per
  rhofac = ((1.0- ecc*COS(nu))/eccfac)**2
  sindec = SIN(ob)*SIN(lambda(n))
  cosdec = SQRT(1.0-sindec**2)
  tandec = sindec/cosdec
  DO j = 1, nlat
    z = -tany(j)*tandec
    IF (z >= 1.0) THEN   ! polar latitudes when there is no sunrise (winter)
      solar(j,n) = 0.0                  
    ELSE          
      IF (z <= -1.0) THEN                ! when there is no sunset (summer)
        solar(j,n) = rhofac * S0(n) * siny(j) * sindec 
      ELSE
        Hzero = ACOS(z)
        solar(j,n) = rhofac/pi*S0(n)*(Hzero*siny(j)*sindec +  &
                                      cosy(j)*cosdec*SIN(Hzero))
      END IF
    END IF
  END DO
END DO
 
END SUBROUTINE

! updaterhs.f90
!****************************************************************************
! Update the Right Hand Side of the equation for the current
! time step. Uses the Temperatures and Forcing from the last time step.
!---------------------------------------------------------------------------

SUBROUTINE UpdateRHS (heatcap, Temp, F, last_F, RHS, LastRHS)

IMPLICIT NONE
INCLUDE 'ebm.inc'

integer  i, j
real,dimension(NX6,NY6):: heatcap, Temp, RHS, LastRHS
real:: F(NX6,NY6), last_F(NX6,NY6)
!--------------------------------------------------------------------------- 

 do j = 1, NY6
   do i = 1, NX6 
      RHS(i,j) = -(4.0*heatcap(i,j)*Temp(i,j)/dt + last_F(i,j) +  &
                    F(i,j) + LastRHS(i,j))
!      if ((i==60) .and. (j==30)) then
!        write(*,*) 'Temp:',Temp(i,j),', lastf:',last_f(i,j), ', f:',f(i,j), ', rhs:', RHS(i,j)
!      endif
   enddo
 enddo

 LastRHS = RHS

END SUBROUTINE

! writeparams.f90
!***************************************************************************
SUBROUTINE WriteParameters (tau_land, tau_snow, tau_sea_ice, tau_mixed_layer, A)!, &
!           Albedo_2D)

IMPLICIT NONE
INCLUDE 'ebm.inc'

real:: tau_land, tau_sea_ice, tau_snow, tau_mixed_layer, A 
integer::  lun
!logical::  Albedo_2D
!B, KlandSP, KlandNP, Keq, Kocean are defined in EBM.inc and are set in EBM.f90 (line ~250)
!-----------------------------------------------------------------------------

	  lun = 2

      write (lun,1) A, B
 1    format(/,' Radiation Constants   A: ',f6.2,' B: ',f6.2)
      write (lun, 65)
 65   format (/,' Diffusion Parameters')
      write (lun,5) KlandSP, KlandNP, Keq, Kocean 
 5    format(' KlandSP: ',f6.2,' KlandNP: ',f6.2,/, &
             '     Keq: ',f6.2,'  Kocean: ',f6.2)
      
      write (lun,9) 
 9    format (/,5x,' Radiative Relaxation Times (C/B) ')
      write (lun,12) tau_sea_ice
 12   format ('    Atmosphere over sea ice :',f6.2,' days')
      write (lun,23) tau_snow
 23   format ('       Atmosphere over snow :',f6.2,' days')
      write (lun,11) tau_land
 11   format ('       Atmosphere over land :',f6.2,' days')
      write (lun, 19) tau_mixed_layer
 19   format (' Atmosphere over mixed layer:',f6.2,' yrs') 

!        write (lun,52)
! 52     format (/,' Lon/Lat Monthly Mean 2D Albedo')

!        write (lun,28) 
! 28     format (/,' Constant Solar Irradiance')
!        write (lun,30) 
! 30     format (' Constant Orbital Parameters')

END SUBROUTINE

     
