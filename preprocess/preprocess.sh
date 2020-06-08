#!/bin/sh

# change to make to run model: 
# 1) change -I\usr\include to refer to the include folder for netcdf for Fortran on your system
# 2) if lnetcdff doesn't exist as a flag for the netcdf for Fortran library in your system, remove it and use: -L/home/username/netcdf/lib and the commented line instead to provide the path to the netcdf library
# 3) change gfortran-7 to Fortran compiler of choice

gfortran-7 -c ../config/parse_config.f90 -o ../config/parse_config.o
cp ../config/parse_config.o ../src/
cp ../config/parse_config.o ../preprocess/
cp ../config/configuration_parser.mod ../src/
cp ../config/configuration_parser.mod ../preprocess/

gfortran-7 ../config/parse_config.f90 extract.f90 prepare_albedo.f90 prepare_geography.f90 preprocessing.f90 -o preprocessing -I/usr/include -lnetcdff
#gfortran-7 ../config/parse_config.f90 extract.f90 prepare_albedo.f90 prepare_geography.f90 preprocessing.f90 -o preprocessing -I/usr/include -L/home/username/netcdf/lib

./preprocessing
mv -f *.nc ../input
rm -rf *~
