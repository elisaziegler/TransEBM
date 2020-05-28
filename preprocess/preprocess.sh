#!/bin/sh

gfortran-7 -c ../config/parse_config.f90 -o ../config/parse_config.o
cp ../config/parse_config.o ../src/
cp ../config/parse_config.o ../preprocess/
cp ../config/configuration_parser.mod ../src/
cp ../config/configuration_parser.mod ../preprocess/
#gfortran-7 extract.f90 -o extract ../config/parse_config.o
#gfortran-7 extract.f90 -o extract
#gfortran-7 prepare_geography.f90 -o prepare_geography ../config/parse_config.o -I/usr/include -L/home/albus/anaconda3/lib -lnetcdff -lnetcdf
#gfortran-7 prepare_albedo.f90 -o prepare_albedo -I/usr/include -L/home/albus/anaconda3/lib -lnetcdff -lnetcdf
#ifort prepare_geography.f90 -o prepare_geography -I/Users/Jiying/netcdf/include -L/Users/Jiying/netcdf/lib -lnetcdff -lnetcdf
#ifort prepare_albedo.f90 -o prepare_albedo -I/Users/Jiying/netcdf/include -L/Users/Jiying/netcdf/lib -lnetcdff -lnetcdf

gfortran-7 ../config/parse_config.f90 extract.f90 prepare_albedo.f90 prepare_geography.f90 preprocessing.f90 -o preprocessing -I/usr/include -L/home/albus/anaconda3/lib -lnetcdff -lnetcdf
#gfortran-7 preprocessing.f90 extract.f90 prepare_albedo.f90 prepare_geography.f90 -o preprocessing ../config/parse_config.o -I/usr/include -L/home/albus/anaconda3/lib -lnetcdff -lnetcdf

#./extract
#./prepare_geography
#./prepare_albedo
./preprocessing
mv -f *.nc ../input
rm -rf *~
