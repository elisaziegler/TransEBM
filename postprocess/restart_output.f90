! Output the simulated results into netcdf

! WIP!!!!!!!!!!!!!!!!!!

subroutine restart_output(filename_base, yrs, write_S, write_c, period)

    implicit none
    include 'netcdf.inc'
    include 'ebm.inc'

    !     This is the name of the data file that will be created.
    character(len=:), allocatable :: BINfile
    character(len=:), allocatable :: filename
    character(len=200)            :: tmp
    character(len=*)              :: filename_base
    character(len=5)              :: yr2str
    !      character*(*) FILE_NAME
    !      parameter (FILE_NAME = '../output/monthly-output.nc')
    integer :: ncid, yrs, yr, period, period_cnt, yr_cnt
    logical :: write_S, write_c

    integer NDIMS 
    parameter (NDIMS = 4)
    integer, parameter :: c_ndims = 3
    integer NRECS, NLVS, NLATS, NLONS
    parameter (NRECS = 12, NLVS=1, NLATS = n_lats, NLONS = n_lons)
    character*(*) LVL_NAME, LAT_NAME, LON_NAME, REC_NAME
    parameter (LVL_NAME='level', LAT_NAME = 'latitude', LON_NAME = 'longitude')
    parameter (REC_NAME = 'time')
    integer lvl_dimid, lon_dimid, lat_dimid, rec_dimid

    real :: res_lat, res_lon

    !     The start and count arrays will tell the netCDF library where to
    !     write our data.
    integer start(NDIMS), count(NDIMS)
    data start /1,1,1,1/
    data count /NLONS,NLATS,NLVS,NRECS/
    integer c_start(c_ndims), c_count(c_ndims)
    data c_start /1,1,1/
    data c_count /NLONS, NLATS, NLVS/

    real lats(NLATS), lons(NLONS)
    integer lon_varid, lat_varid, lvl_varid!, rec_varid

    !     We will create the temperature field.
    character*(*) TEMP_NAME
    parameter (TEMP_NAME='temperature')
    integer temp_varid
    integer dimids(NDIMS), c_dimids(c_ndims)

    !     Heat capacities field.
    character*(*) c_name
    parameter (c_name='heat_capacity')
    integer c_varid

    !     Solcar forcing TOA field.
    character*(*) S_name
    parameter (S_name='toa_incoming_shortwave_flux')
    integer S_varid

    !     We recommend that each variable carry a "units" attribute.
    character*(*) UNITS
    parameter (UNITS = 'units')
    character*(*) TEMP_UNITS, LAT_UNITS, LON_UNITS, lvl_units, S_units, c_units!, rec_units
    parameter (TEMP_UNITS = 'celsius')
    parameter (LAT_UNITS = 'degrees_north')
    parameter (LON_UNITS = 'degrees_east')
    parameter (lvl_units = '1')
    !      parameter (rec_units = 'months since 1950-01-16 12:00:00')
    parameter (S_units = 'W m-2')
    parameter (c_units = 'J m-2 K-1')

    real temp_out(NLONS,NLATS,NLVS,NRECS),temp(NLONS,NLATS)
    real S_out(NLONS,NLATS,NLVS,NRECS),S(NLONS,NLATS)
    real c_out(NLONS,NLATS,NLVS),c(NLONS,NLATS)

    !     Use these to construct some latitude and longitude data for this
    !     example.
    integer START_LAT, START_LON
    parameter (START_LAT = 90.0, START_LON = -180.0)

    !     Loop indices.
    integer lvl, lat, lon, rec, i

    !     Error handling.
    integer retval
    character(len=3):: months(12) = (/'jan','feb','mar','apr','may','jun',  &
                                'jul','aug','sep','oct','nov','dec'/)
    integer ts
    character::  tsnum*2

    tmp = '/stacycode/albus/EBM_output/' // filename_base // '/' // filename_base // '_monthly-output.nc'
    filename = trim(adjustl(tmp))

    !     Create the file. 
    !      retval = nf_create(FILE_NAME, nf_clobber, ncid)
    retval = nf_create(filename, nf_clobber, ncid)

    !     Define the dimensions
    retval = nf_def_dim(ncid, LVL_NAME, NLVS, lvl_dimid)
    retval = nf_def_dim(ncid, LAT_NAME, NLATS, lat_dimid)
    retval = nf_def_dim(ncid, LON_NAME, NLONS, lon_dimid)
    retval = nf_def_dim(ncid, REC_NAME, NF_UNLIMITED, rec_dimid)

    !     Define the coordinate variables
    retval = nf_def_var(ncid, LAT_NAME, NF_REAL, 1, lat_dimid, lat_varid)
    retval = nf_def_var(ncid, LON_NAME, NF_REAL, 1, lon_dimid, lon_varid)

    !     Assign units attributes to coordinate variables.
    retval = nf_put_att_text(ncid, lat_varid, UNITS, len(LAT_UNITS), LAT_UNITS)
    retval = nf_put_att_text(ncid, lon_varid, UNITS, len(LON_UNITS), LON_UNITS)

    !     Define level and time variable
    retval = nf_def_var(ncid, LVL_NAME, NF_REAL, 1, lvl_dimid, lvl_varid)
    !      retval = nf_def_var(ncid, REC_NAME, NF_REAL, 1, rec_dimid, rec_varid)

    !     Assign units attributes to level and time variable.
    retval = nf_put_att_text(ncid, lvl_varid, UNITS, len(lvl_units), lvl_units)
    !      retval = nf_put_att_text(ncid, rec_varid, UNITS, len(rec_units), rec_units)

    !     The dimids array 
    dimids(1) = lon_dimid
    dimids(2) = lat_dimid
    dimids(3) = lvl_dimid
    dimids(4) = rec_dimid

    if (write_c .eqv. .true.) then
        c_dimids(1) = lon_dimid
        c_dimids(2) = lat_dimid
        c_dimids(3) = lvl_dimid
    endif

    !     Define the netCDF variables for the temperature & heat capacity & solar insolation data.
    retval = nf_def_var(ncid, TEMP_NAME, NF_REAL, NDIMS, dimids, temp_varid)
    if (write_c .eqv. .true.) retval = nf_def_var(ncid, c_name, NF_REAL, c_ndims, c_dimids, c_varid)
    if (write_S .eqv. .true.) retval = nf_def_var(ncid, S_name, NF_REAL, NDIMS, dimids, S_varid)

    !     Assign units attributes to the netCDF variables.
    retval = nf_put_att_text(ncid, temp_varid, UNITS, len(TEMP_UNITS), TEMP_UNITS)
    if (write_c .eqv. .true.) retval = nf_put_att_text(ncid, c_varid, UNITS, len(c_units), c_units)
    if (write_S .eqv. .true.) retval = nf_put_att_text(ncid, S_varid, UNITS, len(S_units), S_units)

    !     End define mode.
    retval = nf_enddef(ncid)

    res_lat = 180.0 / NLATS
    res_lon = 360.0 / NLONS

    !     Create real data to write.
    do lat = 1, NLATS
        lats(lat) = START_LAT - (lat - 1) * res_lat ! res_lat = 2.8125
    end do
    do lon = 1, NLONS
        lons(lon) = START_LON + lon * res_lon ! res_lon = 2.8125
    end do

    !     Write the coordinate variable data. This will put the latitudes
    !     and longitudes of our data grid into the netCDF file.
    retval = nf_put_var_real(ncid, lat_varid, lats)
    retval = nf_put_var_real(ncid, lon_varid, lons)

    if (write_c .eqv. .true.) then
        BINfile = '/stacycode/albus/EBM_output/'//filename_base//'/bin/c/heatCapacities.bin'

        open (unit=1, file=BINfile, status='old')
        read (1,*) c
        close(1)
        c_out(:,:,NLVS) = c(:,:)
    !     write heat capacity output to netcdf file 
        retval = nf_put_vara_real(ncid, c_varid, c_start, c_count, c_out)
        IF (retval .NE. NF_NOERR) write(*,*) 'Error: ', nf_strerror(retval)
    endif

    period_cnt = 1
    yr_cnt = 0
    yr_loop: do yr = 1, yrs
    !            BINfile = '../output/'//filename_base//'/bin/S/S_yr-'// yr2str(yr)// '_monthly.bin'

    !            open (unit=1, file=BINfile, status='old')
    !            read (1,*) S
    !            close(1)
    !            S_out(:,:,NLVS,:) = S(:,:,:)
        if (period == period_cnt) then
                do ts = 1, NRECS
        !            write(tmp, '(a, i0, a)') '../output/'//filename_base//'/bin/T/T_yr-', yr, '_' // months(ts)//'.bin'
        !            BINfile = trim(adjustl(tmp))
                    BINfile = '/stacycode/albus/EBM_output/'//filename_base//'/bin/T/T_yr-'// &
                            yr2str(yr)// '_' // months(ts)//'.bin'

                    open (unit=1, file=BINfile, status='old')
                    read (1,*) temp
                    close(1)
        !            close (1,status='delete')
                    temp_out(:,:,NLVS,ts) = temp(:,:)

                    if (write_S .eqv. .true.) then
                            BINfile = '/stacycode/albus/EBM_output/'//filename_base//'/bin/S/S_yr-'// &
                                yr2str(yr)// '_' // months(ts)//'.bin'

                            open (unit=1, file=BINfile, status='old')
                            read (1,*) S
                            close(1)
                !            close (1,status='delete')
                            S_out(:,:,NLVS,ts) = S(:,:)
                    endif
                end do

        !     write temperature output to netcdf file 
                retval = nf_put_vara_real(ncid, temp_varid, start, count, temp_out)

        !     write solar insolation output to netcdf file 
                if (write_S .eqv. .true.) retval = nf_put_vara_real(ncid, S_varid, start, count, S_out)

                yr_cnt = yr_cnt + 1
                start = (/1,1,1,(yr_cnt * NRECS) + 1/)

                period_cnt = 0
        endif
        period_cnt = period_cnt + 1
    end do yr_loop

    start = (/1,1,1,1/)
    count = (/NLONS,NLATS,NLVS,NRECS/)

    !     Close the file. This causes netCDF to flush all buffers and make
    !     sure your data are really written to disk.
    retval = nf_close(ncid)
    !     print *,'Monthly temperature results stored successfully in ', FILE_NAME, '!'
    !     write(2,*)'Monthly temperature results stored successfully in ', FILE_NAME, '!'      
    print *,'Monthly results stored successfully in ', filename, '!'
    write(2,*)'Monthly results stored successfully in ', filename, '!'      

end subroutine restart_output

