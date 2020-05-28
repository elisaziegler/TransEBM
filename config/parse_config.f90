module configuration_parser

implicit none
!private

type, public :: configuration
    character(len=:), allocatable    :: world, ice
    logical                          :: ice_map, ice_ebm
    integer                          :: change_yrs

    logical                          :: co2_forced
    real                             :: co2_value
    character(len=:), allocatable    :: co2_file
    integer                          :: co2_file_len

    integer                          :: initial_yr, run_yrs
    logical                          :: use_equi

    logical                          :: S0_forced
    real                             :: S0_value
    character(len=:), allocatable    :: S0_file
    integer                          :: S0_file_len

    logical                          :: orb_forced
    real                             :: ecc, ob, per
    character(len=:), allocatable    :: orb_src

    logical                          :: orig
    real                             :: a_land, a_land_lat, a_seaice, a_landice, a_ocean, a_ocean_lat
    real                             :: B, CO2_Base, A_Base, CO2_Scaling
    real                             :: kland_SP, kland_NP, kland, keq, kocean
    real                             :: c_atmos, c_soil, c_seaice, c_snow, c_mixed

    integer                          :: period
    logical                          :: write_S, write_c, write_a, write_map

    character(len=:), allocatable    :: wrk_dir, out_dir, id

    logical                          :: restart
    integer                          :: yr_offset
    character(len=:), allocatable    :: restart_dir

    contains
        procedure, public :: parse_config, write_config
end type configuration

contains

subroutine parse_config(config, filename)
    implicit none

    integer :: io_err, iostat 

    character(len=*)                    :: filename
    class(configuration), intent(inout) :: config !has to be class and not type; has to be inout, not just in, if it will be modified

    character(len=200)                  :: world, ice
    logical                             :: ice_map, ice_ebm
    integer                             :: change_yrs

    logical                             :: co2_forced
    real                                :: co2_value
    character(len=200)                  :: co2_file
    integer                             :: co2_file_len

    integer                             :: initial_year, run_years
    logical                             :: use_equi

    logical                             :: S0_forced
    real                                :: S0_value
    character(len=200)                  :: S0_file
    integer                             :: S0_file_len

    logical                             :: orb_forced
    real                                :: ecc, ob, per
    character(len=200)                  :: orb_src

    logical                             :: orig
    real                                :: a_land, a_land_lat, a_seaice, a_landice, a_ocean, a_ocean_lat
    real                                :: B, CO2_Base, A_Base, CO2_Scaling
    real                                :: kland_SP, kland_NP, kland, keq, kocean
    real                                :: c_atmos, c_soil, c_seaice, c_snow, c_mixed

    integer                             :: period
    logical                             :: write_S, write_c, write_a, write_map

    character(len=200)                  :: wrk_dir, out_dir, id

    logical                             :: restart
    integer                             :: yr_offset
    character(len=200)                  :: restart_dir


    namelist/ebm_configuration/ world, ice, ice_map, ice_ebm, change_yrs, co2_forced, co2_value, co2_file, &
        co2_file_len, initial_year, run_years, use_equi, S0_forced, S0_value, S0_file, S0_file_len, &
        orb_forced, ecc, ob, per, orb_src, orig, a_land, a_land_lat, a_seaice, a_landice, a_ocean, a_ocean_lat, &
        CO2_Base, A_Base, CO2_Scaling, B, kland_SP, kland_NP, kland, keq, kocean, c_atmos, c_soil, c_seaice, &
        c_snow, c_mixed, period, write_S, write_c, write_a, write_map, wrk_dir, out_dir, id, restart, &
        yr_offset, restart_dir

    ! Read default configuration (this allows for incomplete config files, since only the parts of the 
    ! namelist that are actually used in filename will be overwritten)
    open(unit = 15, file = '../config/default_config.conf', status = 'old', action = 'read', iostat = io_err)
    if(io_err == 0) then
        read(15, nml = ebm_configuration, iostat = iostat)
    else
        write(*, *) 'Error', io_err, 'occured when trying to open the default configuration file'  
    end if
    close(unit = 15)

    config%world       = trim(adjustl(world))
    config%ice         = trim(adjustl(ice))
    config%ice_map     = ice_map
    config%ice_ebm     = ice_ebm
    config%change_yrs  = change_yrs
    config%co2_forced  = co2_forced
    config%co2_value   = co2_value
    config%co2_file    = trim(adjustl(co2_file))
    config%co2_file_len= co2_file_len
    config%initial_yr  = initial_year
    config%run_yrs     = run_years
    config%use_equi    = use_equi
    config%S0_forced   = S0_forced
    config%S0_value    = S0_value
    config%S0_file     = trim(adjustl(S0_file))
    config%S0_file_len = S0_file_len
    config%orb_forced  = orb_forced
    config%ecc         = ecc
    config%ob          = ob
    config%per         = per
    config%orb_src     = trim(adjustl(orb_src))
    config%orig        = orig
    config%a_land      = a_land
    config%a_land_lat  = a_land_lat
    config%a_seaice    = a_seaice
    config%a_landice   = a_landice
    config%a_ocean     = a_ocean
    config%a_ocean_lat = a_ocean_lat
    config%CO2_Base    = CO2_Base 
    config%A_Base      = A_Base 
    config%CO2_Scaling = CO2_Scaling
    config%B           = B
    config%kland_SP    = kland_SP
    config%kland_NP    = kland_NP
    config%kland       = kland
    config%keq         = keq
    config%kocean      = kocean
    config%c_atmos     = c_atmos 
    config%c_soil      = c_soil
    config%c_seaice    = c_seaice 
    config%c_snow      = c_snow 
    config%c_mixed     = c_mixed
    config%period      = period
    config%write_S     = write_S
    config%write_c     = write_c
    config%write_a     = write_a
    config%write_map   = write_map
    config%wrk_dir     = wrk_dir
    config%out_dir     = out_dir
    config%id          = trim(adjustl(id))
    config%restart     = restart
    config%yr_offset   = yr_offset
    config%restart_dir = restart_dir

    ! Read user-provided configuration
    open(unit = 15, file = filename, status = 'old', action = 'read', iostat = io_err)
    if(io_err == 0) then
        read(15, nml = ebm_configuration, iostat=iostat)
    else
        write(*, *) 'Error', io_err, 'occured when trying to open the configuration file'  
    end if
    close(unit = 15)

    config%world       = trim(adjustl(world))
    config%ice         = trim(adjustl(ice))
    config%ice_map     = ice_map
    config%ice_ebm     = ice_ebm
    config%change_yrs  = change_yrs
    config%co2_forced  = co2_forced
    config%co2_value   = co2_value
    config%co2_file    = trim(adjustl(co2_file))
    config%co2_file_len= co2_file_len
    config%initial_yr  = initial_year
    config%run_yrs     = run_years
    config%use_equi    = use_equi
    config%S0_forced   = S0_forced
    config%S0_value    = S0_value
    config%S0_file     = trim(adjustl(S0_file))
    config%S0_file_len = S0_file_len
    config%orb_forced  = orb_forced
    config%ecc         = ecc
    config%ob          = ob
    config%per         = per
    config%orb_src     = orb_src
    config%orig        = orig
    config%a_land      = a_land
    config%a_land_lat  = a_land_lat
    config%a_seaice    = a_seaice
    config%a_landice   = a_landice
    config%a_ocean     = a_ocean
    config%a_ocean_lat = a_ocean_lat
    config%CO2_Base    = CO2_Base 
    config%A_Base      = A_Base 
    config%CO2_Scaling = CO2_Scaling
    config%B           = B
    config%kland_SP    = kland_SP
    config%kland_NP    = kland_NP
    config%kland       = kland
    config%keq         = keq
    config%kocean      = kocean
    config%c_atmos     = c_atmos 
    config%c_soil      = c_soil
    config%c_seaice    = c_seaice 
    config%c_snow      = c_snow 
    config%c_mixed     = c_mixed
    config%period      = period
    config%write_S     = write_S
    config%write_c     = write_c
    config%write_a     = write_a
    config%write_map   = write_map
    config%wrk_dir     = wrk_dir
    config%out_dir     = out_dir
    config%id          = trim(adjustl(id))
    config%restart     = restart
    config%yr_offset   = yr_offset
    config%restart_dir = restart_dir


    print *,'Successfully read configuration from [', filename,'].'

end subroutine parse_config

subroutine write_config(config, folder)
    implicit none

    integer :: io_err, iostat 

    class(configuration), intent(in)    :: config !has to be class and not type; has to be inout, not just in, if it will be modified
    character(len=*)                    :: folder

    character(len=:), allocatable       :: world, ice
    logical                             :: ice_map, ice_ebm
    integer                             :: change_yrs

    logical                             :: co2_forced
    real                                :: co2_value
    character(len=:), allocatable       :: co2_file
    integer                             :: co2_file_len

    integer                             :: initial_year, run_years
    logical                             :: use_equi

    logical                             :: S0_forced
    real                                :: S0_value
    character(len=:), allocatable       :: S0_file
    integer                             :: S0_file_len

    logical                             :: orb_forced
    real                                :: ecc, ob, per
    character(len=:), allocatable       :: orb_src

    logical                             :: orig
    real                                :: a_land, a_land_lat, a_seaice, a_landice, a_ocean, a_ocean_lat
    real                                :: B, CO2_Base, A_Base, CO2_Scaling
    real                                :: kland_SP, kland_NP, kland, keq, kocean
    real                                :: c_atmos, c_soil, c_seaice, c_snow, c_mixed

    integer                             :: period
    logical                             :: write_S, write_c, write_a, write_map

    character(len=:), allocatable       :: wrk_dir, out_dir, id

    logical                             :: restart
    integer                             :: yr_offset
    character(len=:), allocatable       :: restart_dir

    namelist/ebm_configuration/ world, ice, ice_map, ice_ebm, change_yrs, co2_forced, co2_value, co2_file, &
        co2_file_len, initial_year, run_years, use_equi, S0_forced, S0_value, S0_file, S0_file_len, &
        orb_forced, ecc, ob, per, orb_src, orig, a_land, a_land_lat, a_seaice, a_landice, a_ocean, a_ocean_lat, &
        CO2_Base, A_Base, CO2_Scaling, B, kland_SP, kland_NP, kland, keq, kocean, c_atmos, c_soil, c_seaice, &
        c_snow, c_mixed, period, write_S, write_c, write_a, write_map, wrk_dir, out_dir, id, restart, &
        yr_offset, restart_dir

    world        = trim(adjustl(config%world))
    ice          = trim(adjustl(config%ice))
    ice_map      = config%ice_map
    ice_ebm      = config%ice_ebm
    change_yrs   = config%change_yrs
    co2_forced   = config%co2_forced
    co2_value    = config%co2_value
    co2_file     = trim(adjustl(config%co2_file))
    co2_file_len = config%co2_file_len
    initial_year = config%initial_yr
    run_years    = config%run_yrs
    use_equi     = config%use_equi
    S0_forced    = config%S0_forced
    S0_value     = config%S0_value
    S0_file      = trim(adjustl(config%S0_file))
    S0_file_len  = config%S0_file_len
    orb_forced   = config%orb_forced
    ecc          = config%ecc
    ob           = config%ob
    per          = config%per
    orb_src      = config%orb_src
    orig         = config%orig
    a_land       = config%a_land
    a_land_lat   = config%a_land_lat 
    a_seaice     = config%a_seaice 
    a_landice    = config%a_landice 
    a_ocean      = config%a_ocean 
    a_ocean_lat  = config%a_ocean_lat
    CO2_Base     = config%CO2_Base 
    A_Base       = config%A_Base 
    CO2_Scaling  = config%CO2_Scaling
    B            = config%B
    kland_SP     = config%kland_SP
    kland_NP     = config%kland_NP
    kland        = config%kland
    keq          = config%keq
    kocean       = config%kocean
    c_atmos      = config%c_atmos 
    c_soil       = config%c_soil 
    c_seaice     = config%c_seaice 
    c_snow       = config%c_snow 
    c_mixed      = config%c_mixed
    period       = config%period
    write_c      = config%write_c
    write_S      = config%write_S
    write_a      = config%write_a
    write_map    = config%write_map
    wrk_dir      = trim(adjustl(config%wrk_dir))
    out_dir      = trim(adjustl(config%out_dir))
    id           = trim(adjustl(config%id))
    restart      = config%restart
    yr_offset    = config% yr_offset
    restart_dir  = trim(adjustl(config%restart_dir))

    open(unit = 15, file = folder//'restart_config.conf', status = 'unknown', action = 'write', iostat = io_err) !status='unknown' since it will first be created, later be rewritten
    if(io_err == 0) then
        write(15, nml = ebm_configuration, iostat = iostat)
    else
        write(*, *) 'Error', io_err, 'occured when trying to create the restart configuration file'  
    end if
    close(unit = 15)

!    print *,'Successfully wrote restart-configuration.'

end subroutine write_config


end module configuration_parser

!program test_parser
!    use configuration_parser
!    implicit none
!
!    type(configuration) :: conf
!
!    call conf%parse_config('config.conf')
!    write(*,*) conf%co2_file 
!
!end program test_parser