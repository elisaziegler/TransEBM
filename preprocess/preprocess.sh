#!/bin/sh

gfortran-7 -c ../config/parse_config.f90 -o ../config/parse_config.o
cp ../config/parse_config.o ../src/
cp ../config/parse_config.o ../preprocess/
cp ../config/configuration_parser.mod ../src/
cp ../config/configuration_parser.mod ../preprocess/

gfortran-7 ../config/parse_config.f90 extract.f90 prepare_albedo.f90 prepare_geography.f90 preprocessing.f90 -o preprocessing -I/usr/include -lnetcdff

./preprocessing
mv -f *.nc ../input
rm -rf *~
