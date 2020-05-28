#!/bin/bash

echo $1 #f.ex. $1= /stacywork/albus/EBM0.5/parameterization/

for x in $1*/*/*/*_cdo.nc; do
    echo $x
    outname=${x%_cdo.nc}

    outfile_zf="${outname}_nh_zon_fldmean.nc"

#    cdo -sellonlatbox,-180,180,0,90 $x $outfile
    cdo fldmean -zonmean -sellonlatbox,-180,180,0,90 $x $outfile_zf

    outfile_zf="${outname}_sh_zon_fldmean.nc"

#    cdo -sellonlatbox,-180,180,-90,0 $x $outfile
    cdo fldmean -zonmean -sellonlatbox,-180,180,-90,0 $x $outfile_zf
done