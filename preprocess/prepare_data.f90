subroutine prepare_data(geog_out, file_out, nDims)
    implicit none
    include 'netcdf.inc'
    include '../ebm.inc'

!     This is the name of the data file we will create.
    character(len=*) :: file_out
    integer ncid

!     We are creating a geography map with spatial resolutions of 2.8125 degrees both longitudinal and latitudinal
!     SO we will have a grid of 128 longitude X 65 latitude
    integer :: nDims, nLats, nLons
    parameter (nLats = n_lats, nLons = n_lons)
    character(len=*), parameter :: lat_name = 'latitude'
    character(len=*), parameter :: lon_name = 'longitude'
    integer lon_dimid, lat_dimid

    integer start(NDIMS), count(NDIMS)
    data start /1,1/
    data count /NLONS,NLATS/

    real lats(NLATS), lons(NLONS)
    integer lon_varid, lat_varid

    character*(*) GEOG_NAME
    parameter (geog_NAME='landmask')
    integer GEOG_varid
    integer dimids(NDIMS)

!     We define each variable a "units" attribute.
    character*(*) UNITS
    parameter (UNITS = 'units')
    character*(*) GEOG_UNITS, LAT_UNITS, LON_UNITS
    parameter (GEOG_UNITS = 'landmask: 1. land;  2. sea ice; 3. land ice; 5 ocean. ')
    parameter (LAT_UNITS = 'degrees_north')
    parameter (LON_UNITS = 'degrees_east')

    integer geog_out(NLONS, NLATS)

!     Our grid starts from uppler right corner at longitude 0 degree and latitude 90 degree (0,90)
    integer START_LAT, START_LON
    parameter (START_LAT = 90.0, START_LON = -180.0)

!     Loop indices.
    integer lat, lon, i

!     Error handling.
    integer retval


!     Create grid in real longitude and latitude.
    do lat = 1, NLATS
        lats(lat) = START_LAT - (lat - 1) * 2.8125
    end do
    do lon = 1, NLONS
        lons(lon) = START_LON + lon * 2.8125
    end do

!     Create the file. 
    retval = nf_create(FILE_NAME, nf_clobber, ncid)

!     Define the dimensions
    retval = nf_def_dim(ncid, LAT_NAME, NLATS, lat_dimid)
    retval = nf_def_dim(ncid, LON_NAME, NLONS, lon_dimid)

!     Define the coordinate variables
    retval = nf_def_var(ncid, LAT_NAME, NF_REAL, 1, lat_dimid, lat_varid)
    retval = nf_def_var(ncid, LON_NAME, NF_REAL, 1, lon_dimid, lon_varid)

!     Assign units attributes to coordinate variables.
    retval = nf_put_att_text(ncid, lat_varid, UNITS, len(LAT_UNITS), LAT_UNITS)
    retval = nf_put_att_text(ncid, lon_varid, UNITS, len(LON_UNITS), LON_UNITS)

!     The dimids array 
    dimids(1) = lon_dimid
    dimids(2) = lat_dimid

!     Define the netCDF variables for geography data.
    retval = nf_def_var(ncid, geog_NAME, NF_REAL, NDIMS, dimids, geog_varid)
    
!     Assign units attributes to the netCDF variables.
    retval = nf_put_att_text(ncid, geog_varid, UNITS, len(geog_UNITS), geog_UNITS)

!     End define mode.
    retval = nf_enddef(ncid)
    
!     Write the coordinate variable data. This will put the latitudes
!     and longitudes of our data grid into the netCDF file.
    retval = nf_put_var_real(ncid, lat_varid, lats)
    retval = nf_put_var_real(ncid, lon_varid, lons)

!     write geography output

    retval = nf_put_vara_int(ncid, geog_varid, start, count, geog_out)

!     Close the dat file and make sure your data are really written to disk.
    retval = nf_close(ncid)
    print *
    print *, '-------------------------------------------' 
    print *, 'Preprocess begins:           '
    print *,'SUCCESS in writing geography to ', FILE_NAME, '!'
    
    close(11)
    
end subroutine

