.SUFFIXES:
.SUFFIXES: .o .F

include user_build_options

OBJS = module_apr.o \
       module_hem.o \
       module_emam.o \
       module_io.o \
       driver.o

CMD = driver.exe

FFLAGS =        $(FREESOURCE) $(F90FLAGS)

all:    $(CMD)

driver.exe:     $(OBJS)
	$(COMPILERF90) -o $(@) $(OBJS) $(NETCDFLIB)

.F.o:
	$(RM) $(*).f90
	$(CPP) $(CPPMACROS) $(NETCDFINC) $(*).F > $(*).f90
	$(COMPILERF90) -c $(FFLAGS) $(NETCDFINC) $(*).f90
	$(RM) $(*).f90

clean:
	$(RM) $(OBJS) $(CMD) *.mod *~

driver.exe: driver.o
driver.exe: module_apr.o
driver.exe: module_hem.o
driver.exe: module_emam.o
driver.exe: module_io.o
