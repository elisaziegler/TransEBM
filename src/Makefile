FC=gfortran-7
NAME=EBM
#NA=/home/albus/luna/lib
NI=/usr/include 

all: EBM.f90 grid.f90 app.f90 geography_input.f90 albedo_input.f90 A_value.f90 ice.f90 orbital_params.f90 read_data.f90 write_data.f90 monthly_output.f90 timesteps_output.f90
	${FC} EBM.f90 grid.f90 app.f90 geography_input.f90 albedo_input.f90 A_value.f90 ice.f90 orbital_params.f90 read_data.f90 write_data.f90 monthly_output.f90 timesteps_output.f90 -o ${NAME} ../config/parse_config.o -I${NI} -I/home/luna/TransEBM/TransEBM/config -lnetcdff

clean:
	rm -rf *~


