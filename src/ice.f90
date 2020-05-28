! Melts ice/ freezes sea water in response to the temperature field
!! @param temp: temperature field
!! @param geography: current world map, will be changed if there's freezing or melting
!! @param ice: counts the consecutive timesteps of temperatures being below freezing point over the ocean, and above it for sea ice
!! @param last_ice: ice from the previous timestep
subroutine update_ice(temp, geography, last_ice, pcoalbedo, legendre, update)
    implicit none

    include 'ebm.inc'

    integer :: geography(NX6,NY6), ice(NX6,NY6), last_ice(NX6,NY6)
    real    :: temp(NX6,NY6), pcoalbedo(NX6,NY6), legendre(NY6)
    integer, parameter :: limit = 4 !number of consecutive timesteps temperature has to be below/above freezing for freezing/melting
    integer :: i, j, freeze, melt
    logical :: update
    ! this ensures that no streaks are recorded where there are none
    ice = 0

    freeze = 0
    melt = 0

    do j=1,NY6
        do i=1,NX6
            if((temp(i,j) <= freezing_pt_salt) .and. (geography(i,j) == 5)) then
                if(last_ice(i,j) <= (-1)*(limit-1)) then
!                    write(*,*) 'would have freeze event, update=', update
                    if (update) then 
                        ice(i,j) = 0
                        geography(i,j) = 2
                        pcoalbedo(i,j) = 0.40
                        freeze = freeze + 1
                    endif
                else
                    ice(i,j) = last_ice(i,j) -1
                endif
            elseif ((temp(i,j) >= freezing_pt_salt) .and. (geography(i,j) == 2)) then
                if(last_ice(i,j) >= (limit-1)) then
 !                   write(*,*) 'would have melt event, update =', update
                    if(update) then
                        ice(i,j) = 0
                        geography(i,j) = 5
                        pcoalbedo(i,j) = 0.71 + 0.09 * legendre(j)
                        melt = melt + 1
                    endif
                else
                    ice(i,j) = last_ice(i,j) + 1
                endif
            endif
        enddo
    enddo

    if (update) write(*,*) freeze, ' freeze and ', melt, ' melt events'

    ! FREEZE CASE
    ! if T <= freezing_pt and there isn't ice already, check how long that has been for and whether the 
    ! amount of timesteps required for freezing (given by limit) is reached
    ! if so: freeze; if not: subtract -1 from ice pattern
!    where(temp(:,:) <= freezing_pt_salt .and. geography(:,:) == 5 .and. last_ice(:,:) == (-1)*(limit-1))
!        ice(:,:) = 0
!        geography(:,:) = 2
!    elsewhere (temp(:,:) <= freezing_pt_salt .and. geography(:,:) == 5)
!        ice(:,:) = last_ice(:,:) - 1
!    endwhere

    ! MELT CASE
    ! if T >= freezing_pt and there is ice already, check how long that has been for and whether the 
    ! amount of timesteps required for melting (given by limit) is reached
    ! if so: melt; if not: add + 1 to ice pattern
 !   where(temp(:,:) >= freezing_pt_salt .and. geography(:,:) == 2 .and. last_ice(:,:) == (limit-1))
 !       ice(:,:) = 0
 !       geography(:,:) = 5
 !   elsewhere (temp(:,:) >= freezing_pt_salt .and. geography(:,:) == 2)
 !       ice(:,:) = last_ice(:,:) + 1
 !   endwhere

    last_ice = ice

end subroutine update_ice

!retrieves an ice map from current geography and puts it into ice_mask
subroutine get_ice(geography, ice_mask)
    implicit none

    include 'ebm.inc'

    integer :: geography(NX6,NY6), ice_mask(NX6,NY6)

    where(geography(:,:) == 2 .or. geography(:,:) == 3)
        ice_mask(:,:) = geography(:,:)
    elsewhere
        ice_mask(:,:) = 0 ! this ensures that no ice is recorded where there is none
    endwhere

end subroutine get_ice

subroutine update_albedo(geography, pcoalbedo, legendre)
    implicit none

    integer,parameter:: nx=128, ny=65
    real:: pcoalbedo(nx,ny), legendre(ny)
    integer:: i, j, alb
    integer:: geography(nx,ny)

    do j=1,ny
        do i=1,nx
            alb=geography(i,j)
    !        write(*,*) 'still here? i=', i, ', j=', j, ', alb=', alb
            ! only cover these two cases since these are the only ones that may change due to sea ice formation/melting
            if(alb .eq. 2) pcoalbedo(i,j) = 0.40
            if(alb .eq. 5) pcoalbedo(i,j) = 0.71 + 0.09 * legendre(j)
        enddo
    enddo

end subroutine update_albedo

subroutine calc_legendre(legendre)
    implicit none

    integer,parameter :: nx=128, ny=65
    real :: legendre(ny)
    integer :: j

    legendre = 0.0

    do j = 1, ny
        legendre(j) = 0.5*(3*sin((90.0-(j-1)*2.8125)*3.1415926/180.0)*sin((90.0-(j-1)*2.8125)*3.1415926/180.0)-1)
    enddo

end subroutine calc_legendre

