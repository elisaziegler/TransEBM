! This program ...
! 

program preprocessing
    use configuration_parser
    implicit none

    integer,parameter             :: nx=128, ny=65
    integer                       :: lon, lat
    integer                       :: geo(nx,ny), ice(nx,ny)
    character(len=:), allocatable :: name_albedoDat, name_albedoNc, name_geo, id
    character(len=200)            :: tmp

    !     Configuration file parsing
    type(configuration) :: conf

    print *
    print *, '-------------------------------------------' 
    print *, 'Preprocess begins:           '

!   CHEETAH: LOADING OF CONFIG FILE
    call conf%parse_config('../config/orb_config.conf')

    open(20, file = conf%world, status = 'old') 

    do lat = 1, ny
        read(20,'(128I1)') (geo(lon, lat),lon = 1, nx)
    end do


    ! if land-sea mask and ice are given seperately, load ice and then combine them into one (the way the EBM uses it)
    if (conf%ice_map) then
        open(21, file = conf%ice, status = 'old')
        do lat = 1, ny
            read(21,'(128I1)') (ice(lon, lat),lon = 1, nx)
        end do
        ! substitute ice into map(geo) everywhere, where there is ice in the icemap
        where(ice(:,:) /= 0)
            geo(:,:) = ice(:,:)
        endwhere
        close(21)
    endif
    close(20)

    tmp = conf%id
    id = trim(adjustl(tmp))
    tmp = 'albedo_'//id//'.dat'
    name_albedoDat = trim(adjustl(tmp))
    tmp = 'albedo_'//id//'.nc'
    name_albedoNc = trim(adjustl(tmp))
    tmp = 'geography_'//id//'.nc'
    name_geo = trim(adjustl(tmp))

    call extract(geo, name_albedoDat, conf%orig, conf%a_land, conf%a_land_lat, conf%a_seaice, &
        conf%a_landice, conf%a_ocean, conf%a_ocean_lat)
! both prepare_albedo and geography calculate lon/lat stuff 
    call prepare_albedo(name_albedoDat, name_albedoNc)
    call geography(geo, name_geo)

    print *,'Preprocess DONE! '
    print *, '-------------------------------------------'
    print *     

end program