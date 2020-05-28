!> Creates a new directory if it doesn't exist already.
!! @param dir: path to and name of the directory to be created
subroutine mkdir(dir, verbose)
    implicit none

    character(len=*)    :: dir
    logical             :: exists, verbose
    character(len=200)  :: cmd

    ! Does the directory exist already?
    inquire(file=trim(dir)//'/.', exist=exists )  ! Works with gfortran, but not ifort
!    inquire( directory=newDirPath, exist=dirExists )         ! Works with ifort, but not gfortran

    ! Create directory if it doesn't exist already
    if (exists .eqv. .false.) then
        cmd = 'mkdir -p '//trim(dir)
        if (verbose) write(*,'(a)') "Creating new directory: '"//trim(dir)//"'"
        call system(cmd)
    endif

end subroutine mkdir

character(len=*) function mk_filename(yr, run_yrs, s0, co2, s0_forced, co2_forced, orb_forced, world,&
    write_S, write_c, write_a, write_map)

    implicit none

    integer :: yr, run_yrs
    integer :: s0
    integer :: co2
    logical :: s0_forced, co2_forced, orb_forced
    logical :: write_S, write_c, write_a, write_map
    character(len=*) :: world

    mk_filename = 'eh0kp_u000_s0-0000_co2-000_000000_out0000'!len = 41

    if ((run_yrs >= 1000000)) then
        write(*,*) 'mk_filename has to be adapted to accomodate years/ run_years > 100.000!, Abort.'
        stop
    endif

    if (yr >= 2000) then
        write(mk_filename(3:4), '(i0)') yr-2000
    elseif (yr > - 8000) then
        write(mk_filename(3:3), '(i0)') (-1)*(yr-2000)/1000
    elseif (yr >= -11500) then
        mk_filename(3:4) = 'ol'
    elseif (yr >= -21000) then
        mk_filename(3:4) = 'zk'
    elseif (yr >= -115000) then
        mk_filename(3:4) = 'zj'
    else
        mk_filename(3:4) = 'zi'  
    endif

    if( world == '../preprocess/The_World21.dat') mk_filename(5:5) = 'l'


    if(s0_forced) mk_filename(7:8) = 'f1'
    if(co2_forced) then
        mk_filename(7:7) = 'f'
        mk_filename(9:9) = '1'
    end if
    if(orb_forced) then
        mk_filename(7:7) = 'f'
        mk_filename(10:10) = '1'
    end if

    if (s0 >= 1000) then
        write(mk_filename(15:18), '(i0)') s0
    else
        write(mk_filename(16:18), '(i0)') s0
    end if

    if (co2 >= 100) then
        write(mk_filename(24:26), '(i0)') co2
    else
        write(mk_filename(25:26), '(i0)') co2
    end if

    if (run_yrs >= 100000) then
        write(mk_filename(28:33), '(i0)') run_yrs
    else if (run_yrs >= 10000) then
        write(mk_filename(29:33), '(i0)') run_yrs
    else if (run_yrs >= 1000) then
        write(mk_filename(30:33), '(i0)') run_yrs
    else if (run_yrs >= 100) then
        write(mk_filename(31:33), '(i0)') run_yrs
    else if (run_yrs >= 10) then
        write(mk_filename(32:33), '(i0)') run_yrs
    else
        write(mk_filename(33:33), '(i0)') run_yrs
    end if

    if(write_S) mk_filename(38:38) = '1'
    if(write_c) mk_filename(39:39) = '1'
    if(write_a) mk_filename(40:40) = '1'
    if(write_map) mk_filename(41:41) = '1'

end function mk_filename

subroutine write2Dfile(filename, data)

    implicit none
    include 'ebm.inc'

    real             :: data(NX6, NY6)
    character(len=*) :: filename

    open (unit=7, file=filename)!, status='new')            
    write (7,*) data
    close(7)

end subroutine

subroutine write3Dfile(filename, data)

    implicit none 
    include 'ebm.inc'

    real             :: data(NX6,NY6,NT)
    character(len=*) :: filename

    open (unit=7, file=filename)!, status='new')            
    write (7,*) data
    close(7)

end subroutine

character(len=10) function yr2str(yr)

    implicit none

    integer :: yr

    yr2str = '0000000000'

    if(yr < 10) then
        write(yr2str(10:10), '(i0)') yr
    else if (yr < 100) then
        write(yr2str(9:10), '(i0)') yr
    else if (yr < 1000) then
        write(yr2str(8:10), '(i0)') yr
    else if (yr < 10000) then
        write(yr2str(7:10), '(i0)') yr
    else if (yr < 100000) then
        write(yr2str(6:10), '(i0)') yr
    else if (yr < 1000000) then
        write(yr2str(5:10), '(i0)') yr
    else if (yr < 10000000) then
        write(yr2str(4:10), '(i0)') yr
    else if (yr < 100000000) then
        write(yr2str(3:10), '(i0)') yr
    else if (yr < 1000000000) then
        write(yr2str(2:10), '(i0)') yr
    else 
        write(yr2str, '(i0)') yr
    end if  

end function yr2str