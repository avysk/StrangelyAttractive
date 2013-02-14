FORTRAN=/opt/local/bin/gfortran-mp-4.7
FFLAGS=-pedantic -Wall -std=f2008 -fimplicit-none -Ofast
LIBPGPLOT=-L/opt/local/lib -lpgplot

all: StrangelyAttractive

MODULES=Draw.f08 QuadraticMap.f08
MOD_OBJECTS=$(MODULES:.f08=.o)
SOURCE=StrangelyAttractive.f08
OBJECT=$(SOURCE:.f08=.o)

$(MOD_OBJECTS): %.o: %.f08
	$(FORTRAN) $(FFLAGS) -c $< -o $@

$(OBJECT): $(MOD_OBJECTS)
	$(FORTRAN) $(FFLAGS) -c $(SOURCE) -o $@

StrangelyAttractive: $(OBJECT) $(MOD_OBJECTS)
	$(FORTRAN) $(FFLAGS) $(LIBPGPLOT) -o StrangelyAttractive $(OBJECT) $(MOD_OBJECTS)

clean:
	rm -rf StrangelyAttractive *.o *.mod

run: all
	PGPLOT_PNG_WIDTH=1680 PGPLOT_PNG_HEIGHT=1050 ./StrangelyAttractive

# Gfortran manual said so
%.o: %.mod

.PHONY: all clean run
