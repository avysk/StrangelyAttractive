StrangelyAttractive
===================

Fortran program to find chaotic-behaving 2d quadratic maps

To compile
----------

1. install pgplot from http://www.astro.caltech.edu/~tjp/pgplot/ (or in any other way, like macports);
2. edit top two lines in `Makefile`, specifying there your Fortran compiler and pgplot linking options;
3. `make`

To run
------

`./StrangelyAttractive`

When the progrom finds a chaotic map, you'll be asked for graphics device. Press '?' to see the list. On Mac OS X try `/AQT`
(if you've installed pgplot with Aquaterm support -- strongly recommended). Another interesting option would be
`filename.png/PNG`, which would create `filename.png` in the current directory.

To change behaviour
-------------------

You can edit parameters in the beginning of `StrangelyAttractive.f95`:

- `a_min` and `a_max` specify the range from where the quadratic map coefficients are randomly chosen
- `draw_loops` -- set to `.TRUE.` if you want to see the maps with limit loop shown (boring...)
- `x0` and `y0` -- the initial point; I doubt you can get any different behaviour by changing it
- `total_iterations` -- the amout of points to iterate (only `total_iterations` - `init_iterations` of those will
  be drawn). Increasing this value up to, say, 500000, will usually produce really nice pictures, but
  the drawing will take a lot of time

Do not touch anything else.
