!*************************************************************************
! This is the main program
! Final version: May 29, 2014 at Dickinson College

! A netCDF version of the Energy Balance Model 
!         using the full multigrid solver

!  EBM_netCDF is a free software.
!  You can redistribute it and/or modify it 
!  under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
! 
!  EBM_netCDF is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

!  For compilation, please see Makefile in the src subdirectory.
!  This code has been tested under Ubuntu 12.04LTS, CentOS6.5 and MacOSX
!  using intel fortran compiler and gfortran with netcdf3.6.3 and netcdf4.1. 

!  Users need to modify the netcdf library and include paths in Makefile 
!  to your own installed directories. 
!  For netcdf3.6.3 and below, the -lnetcdff flag is not needed and should be
!  removed from Makefile.

!***************************************************************************
PROGRAM EBM

    use configuration_parser

    IMPLICIT NONE
    INCLUDE 'memory.inc'
    INCLUDE 'ebm.inc'

! ======= variable explanations               ============================

! There are 48 time steps in the year. The astronomical calculations of the 
! Earth's orbit begin at the vernal equinox. This is used to calculate the TOA
! radiation. The first time step is at the vernal equinox. One month is then
! 4 time steps. Model years use the astronomical year, NOT the calendar year.
!
!       Vernal Equinox     1
!       Summer Solstice   13
!       Autumnal Equinox  25
!       Winter Solstice   37
!
!       Month     Time Steps
!        Jan    38, 39, 40, 41
!        Feb    42, 43, 44, 45
!        Mar    46, 47, 48   1
!        Apr     2,  3,  4,  5
!        May     6,  7,  8,  9
!        Jun    10, 11, 12, 13
!        Jul    14, 15, 16, 17
!        Aug    18, 19, 20, 21
!        Sep    22, 23, 24, 25
!        Oct    26, 27, 28, 29
!        Nov    30, 31, 32, 33
!        Dec    34, 35, 36, 37
! 
!  All model output is in binary files. The longitude-latitude grid is
!  128 x 65, with points at both poles. The grid interval is 2.8125
!  degrees. The arrangement of the data is:
!  
!  data(1,1) = 180W, 90N ...... data(128,1) = 177.1875E, 90N
!  ...                        ....
!  data(1,65) = 180W, 90S.......data(128,65) = 177.1875E, 90S

! NX6:  number of grid points in longitude on the finest grid level 
! NY6:  number of grid points in latitude on the finest grid level
! NT:  number of time steps per model year
! rhs(NX6,NY6):  right hand side of equation
! Temp(NX6,NY6):  surface temperatures 
!  Lastrhs(NX6,NY6):  last values of the RHS 
!  F(NX6,NY6,NT):  total radiative forcing 
!  SF(NX6,NY6,NT):  TOA solar radiative forcing 
!  Pcoalbedo(NX6,NY6,NT):  Planetary Coalbedos
!  HeatCap(NX6,NY6):  heat capacities
!  sum(NX6,NY6):  sum over time steps
!  latd(NY6):  latitude (degrees)
!  lond(NX6+1):  longitude (degrees)
!  cost(NY6):  cosine of colatitude
!  Global:  function names
!  GTemp:   global mean temperature
!  NHTemp:   NH mean temperature
!  SHTemp:   SH mean temperature
!  AnnTemp:   global annual mean temp
!  NHAnnTemp:   NH annual mean temp
!  SHAnnTemp:   SH annual mean temp
!  Last_AnnTemp:   last annual temperature
!  tau_land:   relaxation time for land
!  tau_snow:   relaxation time for snow
!  tau_sea_ice:   relaxation time for sea ice
!  tau_mixed_layer:   relaxation time for mixed layer
!  A:   OLR coefficient, A+BT
!  spinup:  model spin-up time
!  geography(NX6,NY6):  masks for the whole globe      
!  RelErr = 2.0e-5:  relative error between years
!  nx(NG), ny(NG):  grid points on each level
!  h(NG):  dx,dy on each grid level
!  geom, GCnp, GCsp:  geometry and terms at poles
!  ecc:  orbital elements, eccentricity
!  ob:                     obliquity 
!  per:                   long of perihelion
!  Solar_Constant:  mean value of solar constant S0 (W m^-2)
!  dt = 1.0/real(NT):  time step as fraction of a year (1/48)
!  dy = pi/real(NY6-1):  increment of latitude in radians
!  B = 2.15:  radiation damping coefficient, A+BT
!  KlandSP:  land diffusion coefficient at South Pole
!  KlandNP:  land diffusion coefficient at North Pole
!  Kland:  land diffusion coefficient at equator
!  Keq:  ocean diffusion coefficient at equator
!  Kocean:  ocean diffusion coefficient at poles 


!--------------------- general EBM variables -----------------------------
    real :: rhs(NX6,NY6)
    real :: Temp(NX6,NY6)
    real :: Lastrhs(NX6,NY6)
    real :: SF(NX6,NY6), last_SF(NX6,NY6)
    real :: solar(NY6,NT)
    real :: Pcoalbedo(NX6,NY6,NT), Pcoalbedo_init(NX6,NY6,NT), legendre(NY6)
    real :: HeatCap(NX6,NY6), HeatCap_init(NX6,NY6)
    real :: sum(NX6,NY6)
    real :: sum_sf(NX6,NY6), sum_coalbedo(NX6,NY6), sum_c(NX6,NY6)!, sum_map(NX6,NY6)
    real :: latd(NY6)
    real :: lond(NX6+1)
    real :: cost(NY6)
    real :: Global
    real :: GTemp
                            
    real :: AnnTemp
    real :: Last_AnnTemp
    real :: tau_land
    real :: tau_snow
    real :: tau_sea_ice
    real :: tau_mixed_layer
    real :: A
    real :: Monthly, ryear
    real :: CO2ppm
    real :: Solar_Constant
    real :: siny(NY6), cosy(NY6), tany(NY6)

    ! S0 variables from configuration file
    logical         :: S0_forced
    character(len=200)            :: tmp
    character(len=:), allocatable :: map, S0_file, S0_filename
    integer :: S0_file_len
    ! array for variable S0 runs and array to temporarily store one row of that array
    ! dimension of S0_oneRun will be (run_years)
    real(kind=8), dimension(:),allocatable   :: S0_oneRun, co2_oneRun
    ! CO2 variables from configuration file
    logical :: co2_forced
    character(len=:), allocatable :: co2_file, co2_filename
    integer :: co2_file_len
    ! time variables from configuration file
    integer:: initial_year, run_years, yrs
    ! orbital variables form configuration file
    logical :: orb_forced
    character(len=:), allocatable :: orb_src
    ! parameterization variables from configuration file
    logical :: orig
    ! output variables from configuration file
    logical :: write_S, write_c, write_a, write_map
    ! run variables from configuration file
    character(len=:), allocatable :: wrk_dir, out_dir, run_id
    integer :: period, period_cnt
    ! restart variable
    logical :: restart
    character(len=:), allocatable :: restart_dir
    ! ice variable
    logical :: ice_ebm
    integer:: yr_loop_start
    logical:: file_exists

    real:: SECNDS, secs, elapsed_time           
    integer:: geography(NX6,NY6), geography_init(NX6,NY6), geo_update(NX6,NY6,12), ice_cnt(NX6,NY6), ice_mask(NX6,NY6)
    integer:: i, j, yr, tstep, year, ts
    integer::  mcount
    real:: tstep2months, tstepsPerMonth, tscount!, monthly_map
    !integer:: Maxyrs
    logical:: Equilibrium, use_equi 
    character:: step*2

    character(len=:), allocatable :: datafile, datafile_sf, datafile_heatCap, datafile_lastRhs, datafile_lastSF, &
                                    datafile_map, datafile_coalbedo, datafile_ice, filename_albedo, &
                                    filename_geo
    character(len=:), allocatable :: startMessage

    character(len=3):: months(12) = (/'jan','feb','mar','apr','may','jun',  &
                                    'jul','aug','sep','oct','nov','dec'/)

    integer :: tstep_solar(NT) = (/38,39,40,41,42,43,44,45,46,47,48,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17, &
                                18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37/)
                                    
    !------------------ Set Some Parameters ----------------------------------
    real, parameter :: RelErr = 2.0e-5   

    !------------------  variables for FMG solver  ---------------------------
    integer,dimension(NG):: nx, ny               
    real:: h(NG)                                 
    real,dimension(NG):: geom, GCnp, GCsp        
    logical:: Converged 

    !------------------  variables for orbital forcing  ----------------------
            
    real::  ecc, ob, per                         

    !------------------  variables for solar forcing  ------------------------
    real::  S0(NT)                            

    !------------------ variable for configuration file parsing --------------
    type(configuration) :: conf, restart_conf

    !----------------- variables for creating output filenames ---------------
    character(len=200)            :: filename_to_be
    character(len=:), allocatable :: filename_monthly
    character(len=:), allocatable :: filename_timesteps
    character(len=41)             :: filename_base
    character(len=41)             :: mk_filename
    character(len=10)             :: yr2str
    integer                       :: initial_co2, initial_s0

    !-----------------  variables for mixed-layer heatflux  ------------------ 

    !-----------------  Old Input Namelist for EBM  -------------------------- 
    integer:: FirstYr 
    logical:: Albedo_2D
                    
    Albedo_2D = .true.

    ! parse configuration file
    ! CHEETAH: SETTING OF CONFIG FILE
    call conf%parse_config('../config/orb_config.conf')
    restart_conf = conf
    restart_conf%restart = .true.

    !*************************************************************************    
    !                        BEGIN EXECUTABLE STATEMENTS
    !************************************************************************* 

    secs = SECNDS(0.0)                 
    tstep2months = 12.0/NT
    tstepsPerMonth = NT/12.0
    !monthly_map = tstepsPerMonth * 2 + tstepsPerMonth * 5 !2 for sea ice and 5 for water, since these are the only changes in geography

    !------------------  Initialize common block parameters  -----------------    
    pi = acos(-1.0)
    dt = 1.0/real(NT)                
    dy = pi/real(NY6-1)               
    ice_cnt = 0

    !------------------------  Initialize some variables  --------------------         

    !CO2ppm=315.0 !AD1950
    !initial_year=1950
    !CO2ppm=315.0 !9kaBP
    !initial_year=-9000
    !CO2ppm=185.0 !21kaBP
    !initial_year=-21000

    tmp = conf%world
    map = trim(adjustl(tmp))
    ice_ebm = conf%ice_ebm
    ice_ebm = .false. !---- since this doesn't work, always set it to false for now
    co2_forced = conf%co2_forced
    tmp = conf%co2_file
    co2_file = trim(adjustl(tmp))
    co2_file_len = conf%co2_file_len
    CO2ppm = conf%co2_value

    orig = conf%orig

    initial_year = conf%initial_yr
    orb_forced = conf%orb_forced
    ecc = conf%ecc
    ob = conf%ob
    per = conf%per
    tmp = conf%orb_src
    orb_src = trim(adjustl(tmp))

    run_years = conf%run_yrs
    FirstYr = run_years
    use_equi = conf%use_equi

    Solar_Constant = conf%S0_value
    S0_forced = conf%S0_forced
    tmp      = conf%S0_file
    S0_file  = trim(adjustl(tmp))
    S0_file_len = conf%S0_file_len
    allocate(S0_oneRun(S0_file_len))
    allocate(co2_oneRun(co2_file_len))

    period = conf%period
    write_S = conf%write_S
    write_c = conf%write_c
    write_a = conf%write_a
    write_map = conf%write_map
    !tmp = conf%out_dir
    !out_dir = trim(adjustl(tmp))
    tmp = conf%id
    run_id = trim(adjustl(tmp))
    tmp = trim(adjustl(conf%wrk_dir))//trim(adjustl(run_id))//'/'
    wrk_dir = trim(adjustl(tmp))
    tmp = trim(adjustl(conf%out_dir))//trim(adjustl(run_id))//'/'
    out_dir = trim(adjustl(tmp))

    if(orig) then
        B = 2.15
        KlandSP = 0.20
        KlandNP = 0.28
        Kland   = 0.65
        Keq     = 0.65
        Kocean  = 0.40
    else
        B = conf%B
        KlandSP = conf%kland_SP
        KlandNP = conf%kland_NP
        Kland   = conf%kland
        Keq     = conf%keq
        Kocean  = conf%kocean
    endif

    if(ice_ebm) then
        call calc_legendre(legendre)
    endif

    restart = conf%restart
    if (restart) then
        yr_loop_start = conf%yr_offset+1
    else
        yr_loop_start = 1
    endif

    ! If S0 is forced, this reads out the noisy S0 values (into S0_oneRun) from the file
    if (S0_forced) then
        inquire(file = S0_file, exist=file_exists)
        if(file_exists .eqv. .false.) then
        write(*,*) 'Checked for file ', S0_file, ', but it was not found. Abort. :('
        stop
        endif
        call read_data(S0_file, S0_oneRun, S0_file_len)
    endif

    ! If CO2 is forced, this reads out the values (into co2_oneRun) from the file
    if (co2_forced) then
        inquire(file = co2_file, exist=file_exists)
        if(file_exists .eqv. .false.) then
            write(*,*) 'Checked for file ', co2_file, ', but it was not found. Abort. :('
            stop
        endif
        call read_data(co2_file, co2_oneRun, co2_file_len)
    endif

    !------------------------  Initialize some arrays  ----------------------- 
    !THESE ARE NEVER ACTUALLY USED!!!
    do j = 1, NY6
    latd(j) = (pi/2.0 - dy*real(j-1))*180./pi    
    cost(j) = cos(dy*real(j-1))                   
    end do
    do i = 1, NX6+1
    lond(i) = dy*real(i-1)*180./pi               
    end do  

    tmp = '../input/geography_'//run_id//'.nc'
    filename_geo = trim(adjustl(tmp))
    tmp = '../input/albedo_'//run_id//'.nc'
    filename_albedo = trim(adjustl(tmp))

    !------------------ Read geography and coalbedos from input --------------
    CALL geography_input(geography, filename_geo)
    CALL albedo_input(Pcoalbedo, filename_albedo)
    geography_init = geography
    Pcoalbedo_init = Pcoalbedo

    !-------------------  Calculate the heat capacities  --------------------- 
    CALL HeatCapacities (HeatCap, geography, tau_land, tau_snow, tau_sea_ice, &
                        tau_mixed_layer, orig, conf%C_atmos, conf%C_soil, &
                        conf%C_seaice, conf%C_snow, conf%C_mixed)
    HeatCap_init = HeatCap

    !---------------------- Setup for the FMG Solver  ------------------------
    CALL FMG_Setup (nx, ny, h, geom, Heatcap, geography, GCnp, GCsp)     

    WRITE(*,*)
    WRITE(*,*)
    WRITE(*,*) '**************************************************************'
    WRITE(*,*) '*****                                                    *****'
    WRITE(*,*) '*****             2D EBM PALEOCLIMATE MODEL              *****'
    WRITE(*,*) '*****                                                    *****'
    WRITE(*,*) '*****          -------- version 1.0.0  --------          *****'
    WRITE(*,*) '*****                                                    *****'
    WRITE(*,*) '*****                    Kelin Zhuang                    *****' 
    WRITE(*,*) '*****                  Dickinson College                 *****'
    WRITE(*,*) '*****                         &                          *****'
    WRITE(*,*) '*****            Gerald R. North and Mark J. Stevens     *****'
    WRITE(*,*) '*****                  Texas A&M University              *****'
    WRITE(*,*) '*****                                                    *****'
    WRITE(*,*) '*****                       2015                         *****'
    WRITE(*,*) '*****                                                    *****'
    WRITE(*,*) '**************************************************************'


    initial_co2 = int(CO2ppm)

    call A_value(CO2ppm, A, orig, conf%CO2_Base, conf%A_Base, conf%CO2_Scaling)

    !-----------------------  Get the orbital elements  ----------------------
    if (orb_src == 'code') then
        call orbital_params(initial_year,ecc,ob,per)
        restart_conf%ecc = ecc !write initial orbital config for restart config in case constant orbital params from code
        restart_conf%ob = ob
        restart_conf%per = per
    endif

    initial_S0 = int(Solar_Constant)
    !----------------- initialize s0 array -----------------------------------
    do ts = 1, NT
        S0(ts) = Solar_Constant                    
    end do

    filename_base = mk_filename(initial_year, run_years, initial_S0, initial_co2, S0_forced, co2_forced, orb_forced, &
        map, write_S, write_c, write_a, write_map)

    restart_conf%restart_dir = trim(adjustl(out_dir))//trim(adjustl(filename_base))

    call mkdir(wrk_dir//filename_base, .true.)
    call mkdir(wrk_dir//filename_base//'/bin', .false.)
    call mkdir(wrk_dir//filename_base//'/bin/T', .false.)
    call mkdir(wrk_dir//filename_base//'/bin/LastRhs', .false.)
    call mkdir(wrk_dir//filename_base//'/bin/LastSF', .false.)
    if (write_S .eqv. .true.) call mkdir(wrk_dir//filename_base//'/bin/S', .false.)
    if (write_c .eqv. .true.) call mkdir(wrk_dir//filename_base//'/bin/c', .false.)
    if (write_a .eqv. .true.) call mkdir(wrk_dir//filename_base//'/bin/coalbedo', .false.)
    if (write_map .eqv. .true.) then
        call mkdir(wrk_dir//filename_base//'/bin/map', .false.)
        call mkdir(wrk_dir//filename_base//'/bin/ice', .false.)
    endif

    tmp = wrk_dir//filename_base//'/bin/T/T_yr-'
    datafile = trim(adjustl(tmp))
    tmp = wrk_dir//filename_base//'/bin/LastRhs/LastRhs_yr-'
    datafile_lastRhs = trim(adjustl(tmp))
    tmp = wrk_dir//filename_base//'/bin/LastSF/LastSF_yr-'
    datafile_lastSF = trim(adjustl(tmp))
    if (write_S .eqv. .true.) then
      tmp = wrk_dir//filename_base//'/bin/S/S_yr-'
      datafile_sf = trim(adjustl(tmp))
    endif
    if (write_c .eqv. .true.) then
      if(ice_ebm .eqv. .false.) then
        tmp = wrk_dir//filename_base//'/bin/c/heatCapacities.bin'
        datafile_heatCap = trim(adjustl(tmp))
        call write2Dfile(datafile_heatCap, HeatCap)
      else 
        tmp = wrk_dir//filename_base//'/bin/c/c_yr-'
        datafile_heatCap = trim(adjustl(tmp))
      endif
    endif
    if (write_a .eqv. .true.) then
      tmp = wrk_dir//filename_base//'/bin/coalbedo/coalbedo_yr-'
      datafile_coalbedo = trim(adjustl(tmp))
    endif
    if (write_map .eqv. .true.) then
      tmp = wrk_dir//filename_base//'/bin/map/map_yr-'
      datafile_map = trim(adjustl(tmp))
      tmp = wrk_dir//filename_base//'/bin/ice/ice_yr-'
      datafile_ice = trim(adjustl(tmp))
    endif

    ! Create working directory and then move input files for geograhpy and albedo into it
    ! as well as copying the configuration to it
    call mkdir(wrk_dir, .false.)
    tmp = 'mv '//trim(adjustl(filename_geo))//' '//wrk_dir//filename_base
!    write(*,*) 'run command: ', trim(adjustl(tmp))
    call system(tmp)
    tmp = 'mv '//trim(adjustl(filename_albedo))//' '//wrk_dir//filename_base
    call system(tmp)
    ! CHEETAH: COPYING OF CONFIG FILE
    tmp = 'cp ../config/orb_config.conf '//wrk_dir//filename_base
    call system(tmp)

    !-------------------------- Write input parameters  ----------------------  
        
    call WriteParameters (tau_land, tau_snow, tau_sea_ice, tau_mixed_layer, A)!, &
!              Albedo_2D)  

    !------------------  Initialize the TOA solar forcing  ------------------- 
    call sinCosTanLat(siny, cosy, tany)
    call calc_insolation(S0, ecc, ob, per, solar, siny, cosy, tany)
    call Solar_Forcing(Pcoalbedo_init, A, solar, SF, tstep_solar(1))
    HeatCap = HeatCap_init
    geography = geography_init
    Pcoalbedo = Pcoalbedo_init

    if (restart) then
      write(*,*) 'Getting restart input'
      tmp = conf%restart_dir    
      restart_dir = trim(adjustl(tmp))
      tmp = restart_dir//'/bin/LastRhs/LastRhs_yr-'//yr2str(conf%yr_offset)//'.bin'
      open (unit=1, file=tmp, status='old')
      read (1,*) LastRhs
      close(1)
      tmp = restart_dir//'/bin/LastSF/LastSF_yr-'//yr2str(conf%yr_offset)//'.bin'
      open (unit=1, file=tmp, status='old')
      read (1,*) last_SF
      close(1)
      write (step, '(i2)') NT 
      tmp = restart_dir//'/bin/T/T_yr-'//yr2str(conf%yr_offset)//'_t'//step//'.bin'
      open (unit=1, file=tmp, status='old')
      read (1,*) Temp
      close(1)
    else
      !---- Initialize the temperature field with constant value everywhere ---- 
      CALL Initial_Temp (Temp)
      LastRhs = 0.0
      call Solar_Forcing(Pcoalbedo_init, A, solar, last_SF, tstep_solar(NT)) !write into last_SF insolation for previous month
    endif

    year = 1
    yrs = run_years
    period_cnt = 1 !previously period_cnt = period (has to be changed in monthly-output & timesteps-output as well)

    sum = 0.0
    sum_sf = 0.0
    sum_coalbedo = 0.0
    sum_c = 0.0
!    sum_map = 0.0
    rhs = 0.0
    GTemp = 0.0                   

    ! output model parameters and global temperature of each step         
    open (unit=2, file=wrk_dir//filename_base//'/Briefing.out', status='replace') 

    write(*, *) '--------------------------------------------------------------'
    tmp = 'Start Energy Balance Model with'
    if (co2_forced) then
      tmp = trim(adjustl(tmp)) //' forced CO2 and'
    else 
      write(tmp, '(a,i0,a)') trim(adjustl(tmp)) // ' constant CO2 = ', initial_co2, 'ppm and'
    endif
    if (s0_forced) then
      tmp = trim(adjustl(tmp)) //' varying solar forcing'
    else
      write(tmp, '(a,i0,a)') trim(adjustl(tmp)) // ' solar constant = ', initial_S0, 'W/m^2'
    endif
    if(orb_forced) tmp = trim(adjustl(tmp)) //' and varying orbital configuration'

    startMessage = trim(adjustl(tmp))
    write(*, *) startMessage
    write(2, *) startMessage

    write (*,24)
    write (2,24)
    24 format(/,'Year  Global Temperature Solar Constant CO2')

    !---------------------- START LOOP OVER MODEL YEARS  --------------------- 
    Equilibrium = .false. 
    DO yr = yr_loop_start, run_years            
      ! Redo everything co2-related if co2 is forced
      if(co2_forced) then
        CO2ppm = co2_oneRun(yr)
        call A_value(CO2ppm, A, orig, conf%CO2_Base, conf%A_Base, conf%CO2_Scaling)
      endif

      ! Redo everything solar forcing related if S0 is forced
      if(S0_forced) then
        Solar_Constant = S0_oneRun(yr)
        do ts = 1, NT
          S0(ts) = Solar_Constant
        end do
      endif

      ! Re-calculate orbital params if they can be forced
      if(orb_forced) then
        call orbital_params(( initial_year + yr),ecc,ob,per)
      endif

      mcount = 1

      call calc_insolation(S0, ecc, ob, per, solar, siny, cosy, tany)

      tscount = 1.0
    !--------------------  START LOOP OVER MODEL TIME STEPS  ----------------- 
      DO tstep = 1, NT             
        Converged = .false.
        if (tstep == NT .and. Equilibrium) then      
          write (*,58) yr-1, AnnTemp                            
          write (2,58) yr-1, AnnTemp
          yrs = yr-1
    58    format (/,'EQUILIBRIUM REACHED AFTER ',i0,' YEARS. GLOBAL TEMP= ',f10.5)
          goto 80                       
        else if (tstep == 1 .and. yr > yr_loop_start) then
          tscount = 1.0
          mcount = 1
          AnnTemp = GTemp/real(NT)
            
          if (use_equi) then
            if (abs(AnnTemp-Last_AnnTemp) <= RelErr) then
              Equilibrium = .true.
            endif
          endif
          if (year < FirstYr) then
            write (*,60) yr-1, AnnTemp, Solar_Constant, CO2ppm
          else
            if (year==FirstYr) print *,'Begin writing output data'
            if (year>FirstYr) then
    
              write (*,60) yr-FirstYr, Anntemp, Solar_Constant, CO2ppm
            else
              write (*,60) yr-1, Anntemp, Solar_Constant, CO2ppm
            end if
          end if
          write (2,60) yr-1, AnnTemp, Solar_Constant, CO2ppm
    60    format (i0,2x,f10.5,3x,'       ',f10.5,'    ',f10.5)

          year = year + 1
          Last_AnnTemp = AnnTemp
          GTemp  = 0.0                
            
        end if

        call Solar_Forcing (Pcoalbedo, A, solar, SF, tstep_solar(tstep))

!This actually should not be necessary yet, since I don't change the geography, if I ever do that, though, there are components of FGM_Setup that depend on the HeatCap 
! => would need to entangle that mess
        CALL HeatCapacities (HeatCap, geography, tau_land, tau_snow, tau_sea_ice, &
                            tau_mixed_layer, orig, conf%C_atmos, conf%C_soil, &
                            conf%C_seaice, conf%C_snow, conf%C_mixed)

!        write(*,*) tstep
        CALL UpdateRHS (HeatCap, Temp, SF, last_SF, rhs, LastRhs) ! also sets LastRhs=rhs at end
!        if (tstep == 1) then
!          write(*,*) rhs
!        endif

        CALL FMG_Solver (nx, ny, h, geom, GCnp, GCsp, Converged, rhs, Temp, .FALSE.)
        if (.NOT.Converged) then
            write (*,61) 
            write (*,59) yr, tstep 
    59    format ('Year =',i7,'Time Step = ',i2)
    61    format ('NO Convergence within max number of V cycles')
          STOP
        end if
    !---------------------    run time step data  ----------------------------
        if (period_cnt == period) then !if(run_years.or.Equilibrium) then     
          write (step, '(i2)') tstep 
          if (tstep < 10) step(1:1) = '0'!put 0 in front of steps y 10 => 1 becomes 01,...
          call write2Dfile(datafile//yr2str(yr)//'_t'//step//'.bin', Temp)
          if (write_map) call write2Dfile(datafile_map//yr2str(yr)//'_t'//step//'.bin', geography)
          if (write_a) call write2Dfile(datafile_coalbedo//yr2str(yr)//'_t'//step//'.bin', Pcoalbedo(:,:,tstep))
          if (write_S) call write2Dfile(datafile_sf//yr2str(yr)//'_t'//step//'.bin', SF)
          if (write_c) call write2Dfile(datafile_heatCap//yr2str(yr)//'_t'//step//'.bin', HeatCap)

    !      sum = sum + 0.25*Temp !b/c 4 values are averaged for monthly temps! so for a different NT this has to change
          sum = sum + tstep2months * Temp
          if (write_S) sum_sf = sum_sf + tstep2months * SF(:,:)
          if (write_a) sum_coalbedo = sum_coalbedo + tstep2months * Pcoalbedo(:,:,tstep)
          if (write_c .and. ice_ebm) sum_c = sum_c + tstep2months * HeatCap(:,:)
!          if (write_map) sum_map = sum_map + geography(:,:)

    !      if (mod(tscount,4.0) == 0.0) then ! modulus 4 because that corresponds to one month having past (since NT=48)
          if (mod(tscount,tstepsPerMonth) == 0.0) then ! modulus 4 because that corresponds to one month having past (since NT=48)
            call write2Dfile(datafile//yr2str(yr)//'_'//months(mcount)//'.bin', sum)
            if (write_S) call write2Dfile(datafile_sf//yr2str(yr)//'_'//months(mcount)//'.bin', sum_sf)
            if (write_a) call write2Dfile(datafile_coalbedo//yr2str(yr)//'_'//months(mcount)//'.bin', sum_coalbedo)
            if (write_c .and. ice_ebm) call write2Dfile(datafile_heatCap//yr2str(yr)//'_'//months(mcount)//'.bin', sum_c)

            Monthly = Global (sum, z(iaw(NG)), 0)
            ryear = real(year) + 0.041667 + 0.083333*real(mcount-1)
            mcount = mcount + 1
            sum = 0.0
            sum_sf = 0.0
            sum_coalbedo = 0.0
            sum_c = 0.0
          end if

          if(tstep == NT) then
            call write2Dfile(datafile_lastRhs//yr2str(yr)//'.bin', LastRhs)
            call write2Dfile(datafile_lastSF//yr2str(yr)//'.bin', SF)!last_SF=SF has not been set yet
            restart_conf%yr_offset = yr
            call restart_conf%write_config(trim(adjustl(wrk_dir))//trim(adjustl(filename_base))//'/')
            call restart_conf%write_config('../config/') !Comment this line for parallelization...for now it is necessary for restarts, though

            if(write_map) then
                call get_ice(geography, ice_mask) !retrieves an ice map from current geography and puts it into ice_mask
                call write2Dfile(datafile_ice//yr2str(yr)//'.bin', ice_mask)
            endif

            period_cnt = 0
            tscount = 0.0
            mcount = 1
          end if
        end if !write data if period == period_cnt

        if (ice_ebm .and. mod(tstep,24) == 0 .and. yr>=5) then
          call update_ice(Temp, geography, ice_cnt, Pcoalbedo, legendre, .true.)
!          call update_albedo(geography, Pcoalbedo, legendre)
        elseif (ice_ebm .and. yr>=5) then
          call update_ice(Temp, geography, ice_cnt, Pcoalbedo, legendre, .false.)
!          call update_albedo(geography, Pcoalbedo, legendre)
        endif

        GTemp =  GTemp  + Global (Temp, z(iaw(NG)), 0)     
        tscount = tscount + 1.0
        last_SF = SF

      END DO !model time steps

      period_cnt = period_cnt + 1                   
    END DO !model years

    80 elapsed_time = SECNDS(secs)      

    if (.NOT.use_equi) then
      write(*, 81) AnnTemp, yrs
      write(2, 81) AnnTemp, yrs
    81 format (/,'GLOBAL TEMPERATURE = ', f8.4, ' AFTER ',i0,' YEARS!')
    else if (.NOT.Equilibrium) then
      write (*,82) yrs, AnnTemp
      write (2,82) yrs, AnnTemp
    82 format (/,' WARNING: EQUILIBRIUM NOT REACHED AFTER ',i0,' YEARS!!!!', &
              /,'         GLOBAL TEMPERATURE = ',f8.4)
    end if

    write (*,90) elapsed_time/60.
    write (2,90) elapsed_time/60. 
    90 format(/,' Elapsed time:',f10.2,' minutes') 

    filename_monthly = wrk_dir // filename_base // '/' // filename_base // '_monthly-output.nc'

    filename_timesteps = wrk_dir // filename_base // '/' // filename_base // '_timesteps-output.nc'

    call monthly_output(filename_base, yrs, yr_loop_start, conf)
    call timesteps_output(filename_base, yrs, yr_loop_start, conf)

    close (2)


    if (allocated(S0_oneRun)) deallocate(S0_oneRun)
    if (allocated(co2_oneRun)) deallocate(co2_oneRun)

END PROGRAM EBM


