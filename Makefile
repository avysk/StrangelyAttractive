all: StrangelyAttractive

StrangelyAttractive: StrangelyAttractive.f95
	/opt/local/bin/gfortran-mp-4.5 -L/opt/local/lib -lpgplot -o "StrangelyAttractive" StrangelyAttractive.f95

clean:
	rm -rf StrangelyAttractive

.PHONY: all clean
