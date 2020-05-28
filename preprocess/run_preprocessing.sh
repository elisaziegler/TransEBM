#!/bin/sh

gfortran-7 preprocessing.f90 extract.f90 prepare_albedo.f90 prepare_geography.f90 -o preprocessing ../config/parse_config.o -I/usr/include -L/home/albus/anaconda3/lib -lnetcdff -lnetcdf

./preprocessing
mv -f *.nc ../input
rm -rf *~
