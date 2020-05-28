#!/bin/bash

for x in $1*/*_monthly-output.nc; do
    echo $x

    # remove "monthly-output.nc" from string
    outname=${x%monthly-output.nc}
    outname_fld="${outname}fldmean.nc"
    outname_zon="${outname}zonmean.nc"
    outname_cdo="${outname}cdo.nc"
    if [ ! -f $outname_cdo ]
    then
        cdo setlevel,1 -settaxis,0-01-16,12:00:00,1mon $x $outname_cdo
#    else 
#        echo $outname_cdo already exists.
    fi

    #fldmean => yrly global weighted mean
    # is timmean really all that useful? For equi runs it will average over the upcycling towards the equi which we are not interested in;
    # for transient runs it will remove the temporal development, i.e. exactly what we're interested in =>no timmean!!!!!!
    if [ ! -f $outname_fld ]
    then
        cdo fldmean $outname_cdo $outname_fld
#    else 
#        echo $outname_fld already exists.
    fi

    #zonmean => averaged over lon for plots along latitude
    if [ ! -f $outname_zon ]
    then
        cdo zonmean $outname_cdo $outname_zon
#    else 
#        echo $outname_zon already exists.
    fi

#     outname_zonfld="${outname}zonfldmean.nc"
#    if [ ! -f $outname_zonfld ]
#    then
#        cdo fldmean $outname_zon $outname_zonfld
##    else 
##        echo $outname_zon already exists.
#    fi


    #for std
#    outname_zonstd="${outname}zonstd.nc"
#    if [ ! -f $outname_zonstd ]
#    then
#        cdo zonstd $outname_cdo $outname_zonstd
##    else 
##        echo $outname_zonstd already exists.
#    fi

    outname_yrmean="${outname}yrmean.nc"
    if [ ! -f $outname_yrmean ]
    then
        cdo yearmean $outname_cdo $outname_yrmean
#    else 
#        echo $outname_yrmean already exists.
    fi

    outname_fld_yrmean="${outname}fld_yrmean.nc"
    if [ ! -f $outname_fld_yrmean ]
    then
        cdo fldmean $outname_yrmean $outname_fld_yrmean
#    else 
#        echo $outname_fld_yrmean already exists.
    fi

#    outname_min_yrmean="${outname}min_yrmean.nc"
#    if [ ! -f $outname_min_yrmean ]
#    then
#        cdo fldmin $outname_yrmean $outname_min_yrmean
##    else 
# #       echo $outname_min_yrmean already exists.
#    fi

#    outname_max_yrmean="${outname}max_yrmean.nc"
#    if [ ! -f $outname_max_yrmean ]
#    then
#        cdo fldmax $outname_yrmean $outname_max_yrmean
##    else 
# #       echo $outname_max_yrmean already exists.
#    fi

    outname_zon_yrmean="${outname}zon_yrmean.nc"
    if [ ! -f $outname_zon_yrmean ]
    then
        cdo zonmean $outname_yrmean $outname_zon_yrmean
#    else 
 #       echo $outname_zon_yrmean already exists.
    fi

    outfile_zf="${outname}nh_zon_fldmean.nc"

#    cdo -sellonlatbox,-180,180,0,90 $x $outfile
    cdo fldmean -zonmean -sellonlatbox,-180,180,0,90 $outname_cdo $outfile_zf

    outfile_zf="${outname}sh_zon_fldmean.nc"

#    cdo -sellonlatbox,-180,180,-90,0 $x $outfile
    cdo fldmean -zonmean -sellonlatbox,-180,180,-90,0 $outname_cdo $outfile_zf


    #seasonal data (Seasons: Mar-Apr-May, Jun-Jul-Aug, Sep-Oct-Nov, Dec-Jan-Feb)
#    cdo timselmean,3,2 infile outfile
    #or (Seasons: Jan-Feb-Mar, Apr-May-Jun, Jul-Aug-Sept, Oct-Nov-Dec)
    # use this because sometimes we might not have consecutive years, so other seasonal means would get messy & very wrong
#    outname_seas="${outname}seasonal.nc"    
#    if [ ! -f $outname_seas ]
#    then
#        cdo timselmean,3 $outname_cdo $outname_seas
##    else 
##        echo $outname_seas already exists.
#    fi
#    #or
##    cdo splitseas $outname_cdo $outname_cdo

    # fldmean for seasonal data
#    outname_seasmean="${outname}seas_fldmean.nc"    
#    if [ ! -f $outname_seasmean ]
#    then
#        cdo fldmean $outname_seas $outname_seasmean
##    else 
##        echo $outname_seasmean already exists.
#    fi

    # zonmean for seasonal data
#    outname_seaszonmean="${outname}seas_zonmean.nc"    
#    if [ ! -f $outname_seaszonmean ]
#    then
#        cdo zonmean $outname_seas $outname_seaszonmean
##    else 
##        echo $outname_seaszonmean already exists.
#    fi

    #fldmin and fldmax 
#    outname_fldmin="${outname}fldmin.nc"    
#    if [ ! -f $outname_fldmin ]
#    then
#        cdo fldmin $outname_cdo $outname_fldmin
##    else 
##        echo $outname_fldmin already exists.
#    fi

#    outname_fldmax="${outname}fldmax.nc"    
#    if [ ! -f $outname_fldmax ]
#    then
#        cdo fldmax $outname_cdo $outname_fldmax
##    else 
##        echo $outname_fldmax already exists.
#    fi

done
