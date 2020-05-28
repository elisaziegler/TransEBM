subroutine read_data(filename, data, len)
    implicit none

    integer                         :: len, io_status, io_err, i
    character(len=*)                :: filename
    real(kind=8), dimension(len) :: data

    open(unit = 21, file = filename, status = 'old', action = 'read', iostat = io_err)

    i = 0

    if (io_err == 0) then
        data_loop: do
            i = i + 1
!            read(21, '(f10.5)', iostat = io_status) data(i)
            read(21, *, iostat = io_status) data(i)
            if (io_status /= 0) then
                if (i < len) then
                    write(*,*) 'File ', filename, 'does not contain enough data for the whole run of ', len, ' years. Aborting.'
                    ! TODOOOOO: Do I want to stop or do I just want to fill with last value over and over or start from the beginning?
                    stop
                end if
                exit data_loop !end of file reached
!            else !sth left in the file to read
!                data(i) = 
!                read(21, *) data(i)
            end if
            if (i == len) then !data is full (has as much data as is needed)
                exit data_loop 
            end if
        end do data_loop
    else
        write(*, *) 'Error', io_err, 'occured when trying to open the configuration file'  

    end if

!    write(*, *) 'This is inside the array now:'
!    write(*, *) data

    close(unit = 21)

end subroutine read_data

!program try_it 
    ! run with: gfortran-7 read_data.f90 -i read_data.o ../config/parse_config-o -I/home/albus/Documents/Models/EBM/Model/config
    ! then: ./read_data.o
!    use configuration_parser

!    implicit none

!    integer :: yrs
!    integer :: i, S0s
!    character(len=:), allocatable :: filename
!    character(len=100):: filename_to_be

!    real(kind=8), dimension(:), allocatable :: data
    !TODOOOOOOOOOOO: will ich hier wirklich kind hardcoden...oder wäre es besser die Daten einfach in Fortran zu generieren...und dann müssten sie
    !compiler-spezifisch ja einfach die selbe Präzision kriegen und ich habe kein Problem
    !oder: stört mich die 4./5. Nachkommastelle überhaupt? 
!    real, dimension(:,:),allocatable :: allData 
!    real :: s0
!    type(configuration) :: conf
!    call conf%parse_config('../config/config.conf')

!    S0s = int((conf%S0_value(2) - conf%S0_value(1)) / conf%S0_value(3)) + 1
!    write(*,*) 'S0s: ', S0s
!    yrs = conf%run_yrs
!    allocate(data(yrs))
!    allocate(allData(yrs, S0s))

!    do i = 0, S0s - 1, 1
!        s0 = conf%S0_value(1) + i * conf%S0_value(3)
!        write(filename_to_be, '(a, i0, a, i0, a)') '../config/solarConstant/s0_mu-', int(s0), '_sigma-', int(conf%S0_sigma), &
!            '_yrs-100.txt'
!        filename = trim(adjustl(filename_to_be))
!        call read_data(filename, data, yrs)
!        write(*, *) 'This is inside the array now:'
!        write(*, *) data

!        allData(:,i+1) = real(data, kind = 4)
!    end do

!    write(*,*) 'all data: ', allData
!    deallocate(allData)
!    deallocate(data)
!end program try_it