FC     = gfortran
FFLAGS = -std=f2003 -pedantic \
         -Wall -Wno-aliasing -Wsurprising -Warray-bounds \
         -pedantic-errors
EXEC = ./sample_program.exe

SRC = logging_mod.f90 sample_program.f90

OBJ = $(SRC:.f90=.o)
MOD = $(SRC:.f90=.mod)

.SUFFIXES: .f90 .o .mod

.f90.o:
	$(FC) $(FFLAGS) -c $<

.f90.mod:
	$(FC) $(FFLAGS) -c $<

all: $(EXEC)

$(EXEC): $(SRC)
	$(FC) $(FFLAGS) -o $(EXEC) $(SRC) $(LIBS)

clean:
	/bin/rm -f $(EXEC) $(OBJ) $(MOD)
