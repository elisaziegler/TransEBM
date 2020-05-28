!-----------> EBM.inc - Include file for the EBM and FMG Solver <-------------

integer,parameter:: NX6=128, NY6=65, NT=48, n_lons = 128, n_lats = 65
real, parameter :: freezing_pt_salt = -2.0


! NX6 is the number of grid points in longitude on the finest grid level 
! NY6 is the number of grid points in latitude on the finest grid level
! NT is the number of time steps per model year
  
real:: pi               ! pi = circumference/(2*radius)
real:: dt               ! time step interval
real:: dy               ! latitude increment
real:: Keq              ! coefficient for diffusion at equator 
real:: Kocean           ! coefficient for ocean diffusion 
real:: Kland            ! coefficient for land diffusion 
real:: KlandNP          ! coefficient for land diffusion 
real:: KlandSP          ! coefficient for land diffusion 
real:: B                ! radiation damping 

common /constants/pi, dt, dy, Kocean, Kland, KlandNP, KlandSP, Keq, B
!------------------------------------------------------------------------------
