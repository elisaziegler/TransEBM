# change to make to run model: 
# 1) change -I/home/username/TransEBM/config to the path to the config folder on your system
# 2) change NI=.. to the include folder for netcdf for Fortran
# 3) if lnetcdff doesn't exist as a flag for the netcdf for Fortran library in your system, remove it and use: -L${NA} instead and provide the path as NA=...
# 4) change FC to your Fortran compiler of choice

FC=gfortran-7
NAME=EBM
#NA=/home/username/netcdf/lib
NI=/usr/include 

all: EBM.f90 grid.f90 app.f90 geography_input.f90 albedo_input.f90 A_value.f90 ice.f90 orbital_params.f90 read_data.f90 write_data.f90 monthly_output.f90 timesteps_output.f90
	${FC} EBM.f90 grid.f90 app.f90 geography_input.f90 albedo_input.f90 A_value.f90 ice.f90 orbital_params.f90 read_data.f90 write_data.f90 monthly_output.f90 timesteps_output.f90 -o ${NAME} ../config/parse_config.o -I${NI} -I/home/username/TransEBM/config -lnetcdff

clean:
	rm -rf *~


