FC=gfortran
FFLAGS=-O3 -Wall -Wextra
PROG1=part1.f90 
PROG2=part2.f90
MOD=mergesortmod.f90 stringmod.f90
SRC=$(MOD)
OBJ=${SRC:.f90=.o}
BASE1=${PROG1:.f90=}
BASE2=${PROG2:.f90=}

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

$(BASE1): $(OBJ) $(PROG1:.f90=.o)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(PROG1:.f90=.o)

$(BASE2): $(OBJ) $(PROG2:.f90=.o)
	$(FC) $(FFLAGS) -o $@ $(OBJ) $(PROG2:.f90=.o)

all: $(BASE1) $(BASE2)

clean:
	rm -f *.o *.mod $(BASE1) $(BASE2)