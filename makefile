#makefile for sllod_2d
#dont forget to use tabs!!
#spaces are significant!

# define default settings
SHELL = /bin/sh
BINROOT =$(PWD)
EX = md_filter.exe

#---- intel F95 compiler
 FC = gfortran -O3  
#FC = gfortran   
  
#SWITCH = -g -fbounds-check  
SWITCH = -g   

OBJECTS = header_file.o md_filter.o read_inputdata.o initialise_unit_cell.o initialise_positions.o\
initialise_velocities.o random_number.o rhs.o rk4.o boundary_conditions.o calculate_energies.o calculate_profile_planes.o\
profiles.o average_profiles.o write_results_to_file.o create_scatterers.o lucy_1d.o lucy.o\
density.o
 
# Makefile
$(EX): $(OBJECTS)
	$(FC) -o $(EX) $(SWITCH) $(OBJECTS)

header_file.mod: header_file.o header_file.f90
	$(FC) -c $(SWITCH) header_file.f90

header_file.o: header_file.f90
	$(FC) -c $(SWITCH) header_file.f90

md_filter.o: header_file.mod md_filter.f90
	$(FC) -c $(SWITCH) md_filter.f90

read_inputdata.o: header_file.mod read_inputdata.f90
	$(FC) -c $(SWITCH) read_inputdata.f90

initialise_unit_cell.o: header_file.mod initialise_unit_cell.f90 
	$(FC) -c $(SWITCH) initialise_unit_cell.f90

initialise_positions.o: header_file.mod initialise_positions.f90 
	$(FC) -c $(SWITCH) initialise_positions.f90

initialise_velocities.o: header_file.mod initialise_velocities.f90 
	$(FC) -c $(SWITCH) initialise_velocities.f90

random_number.o: header_file.mod random_number.f90 
	$(FC) -c $(SWITCH) random_number.f90

rhs.o: header_file.mod rhs.f90 
	$(FC) -c $(SWITCH) rhs.f90

rk4.o: header_file.mod rk4.f90 
	$(FC) -c $(SWITCH) rk4.f90

boundary_conditions.o: header_file.mod boundary_conditions.f90 
	$(FC) -c $(SWITCH) boundary_conditions.f90

calculate_energies.o: header_file.mod calculate_energies.f90 
	$(FC) -c $(SWITCH) calculate_energies.f90

calculate_profile_planes.o: header_file.mod calculate_profile_planes.f90 
	$(FC) -c $(SWITCH) calculate_profile_planes.f90

profiles.o: header_file.mod profiles.f90 
	$(FC) -c $(SWITCH) profiles.f90

average_profiles.o: header_file.mod average_profiles.f90 
	$(FC) -c $(SWITCH) average_profiles.f90

write_results_to_file.o: header_file.mod write_results_to_file.f90 
	$(FC) -c $(SWITCH) write_results_to_file.f90

create_scatterers.o: header_file.mod create_scatterers.f90 
	$(FC) -c $(SWITCH) create_scatterers.f90

lucy_1d.o: header_file.mod lucy_1d.f90 
	$(FC) -c $(SWITCH) lucy_1d.f90

lucy.o: header_file.mod lucy.f90 
	$(FC) -c $(SWITCH) lucy.f90

density.o: header_file.mod density.f90 
	$(FC) -c $(SWITCH) density.f90

%: %.o
	$(FC) -c $(SWITCH) $< 

clean:
	rm -f *.o *.mod *.MOD
