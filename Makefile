FORTRAN=/opt/local/bin/gfortran-mp-4.5
LIBPGPLOT=-L/opt/local/lib -lpgplot

all: StrangelyAttractive

SOURCES=StrangelyAttractive.f95 Draw.f95 QuadraticMap.f95
OBJECTS=$(SOURCES:.f95=.o)

StrangelyAttractive: $(OBJECTS)
	$(FORTRAN) $(LIBPGPLOT) -o StrangelyAttractive $(OBJECTS)

$(OBJECTS): %.o: %.f95
	$(FORTRAN) -c $< -o $@

clean:
	rm -rf StrangelyAttractive *.o

.PHONY: all clean
