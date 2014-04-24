StrangelyAttractive
===================

Fortran program to find chaotic-behaving 2d quadratic maps

To compile
----------

0. make sure you have Fortran compiler with Fortran 2008 support. I know that
   gfortran 4.5 doesn't work, and gfortran 4.8 works.
1. install pgplot from http://www.astro.caltech.edu/~tjp/pgplot/ (or in any
   other way, like macports);
2. edit top two lines in `Makefile`, specifying there your Fortran compiler and
   pgplot linking options;
3. `make`

To run
------

`./StrangelyAttractive`

When the progrom finds a chaotic map, you'll be asked for graphics device.
Press '?' to see the list. On Mac OS X try `/AQT` (if you've installed pgplot
with Aquaterm support -- strongly recommended). Another interesting option
would be `filename.png/PNG`, which would create `filename.png` in the current
directory.

To change behaviour
-------------------

You can edit parameters in the beginning of `StrangelyAttractive.f08`:

- `a_min` and `a_max` specify the range from where the quadratic map
  coefficients are randomly chosen
- `draw_loops` -- set to `.TRUE.` if you want to see the maps with limit loop
  shown (boring...)
- `x0` and `y0` -- the initial point; I doubt you can get any different
  behaviour by changing it
- `total_iterations` -- the number of iterations to calculate; you probably
  do not want to touch this, unless your machine is really slow
- `last_draw_iteration` -- the last iteration to draw (in the drawing the
  first `init_iterations` will be skipped). Increasing this value up to, say,
  500000, will usually produce really nice pictures, but the drawing will take
  a lot of time (but if you use png renderer and not on-screen one, it's
  very fast). Should be less or equal than `total_iterations`.

Do not touch anything else.

Notes on output devices
-----------------------

You may find the following environmental variables useful:

- `PGPLOT_DEV` sets the default output device
- `PGPLOT_PNG_WIDTH` and `PGPLOT_PNG_HEIGHT` specify the size of PNG image
   (pgplot's default is 850 x 680)

