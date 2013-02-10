FORTRAN=/opt/local/bin/gfortran-mp-4.7 -pedantic -Wall -std=f2008
LIBPGPLOT=-L/opt/local/lib -lpgplot

all: StrangelyAttractive

SOURCES=StrangelyAttractive.f08 Draw.f08 QuadraticMap.f08
OBJECTS=$(SOURCES:.f95=.o)

StrangelyAttractive: $(OBJECTS)
	$(FORTRAN) $(LIBPGPLOT) -o StrangelyAttractive $(OBJECTS)

$(OBJECTS): %.o: %.f95
	$(FORTRAN) -c $< -o $@

clean:
	rm -rf StrangelyAttractive *.o

run: all
	PGPLOT_PNG_WIDTH=1680 PGPLOT_PNG_HEIGHT=1050 ./StrangelyAttractive

.PHONY: all clean run
