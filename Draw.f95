SUBROUTINE DRAW(m, x, y)
IMPLICIT NONE

INTEGER, INTENT(IN) :: m
REAL, DIMENSION(:), INTENT(IN) :: x, y

! add that amount of space around the drawing
REAL, PARAMETER :: space = 0.05

REAL :: xmin, xmax, ymin, ymax, deltax, deltay

INTEGER :: ier, PGBEG

! Init pgplot library
ier = PGBEG(0, '?', 1, 1)
if (ier.NE.1) STOP

xmin = MINVAL(x)
xmax = MAXVAL(x)
ymin = MINVAL(y)
ymax = MAXVAL(y)
deltax = space * (xmax - xmin)
deltay = space * (ymax - ymin)
xmin = xmin - deltax
xmax = xmax + deltax
ymin = ymin - deltay
ymax = ymax + deltay

CALL PGENV(xmin, xmax, ymin, ymax, 0, 0)

! We want black on white
CALL PGSCR(0, 1., 1., 1.)
CALL PGSCR(1, 0., 0., 0.)
CALL PGERAS
! We just erased coordinate box; redraw it
CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0)

WRITE (*, '(A)') 'Drawing, please wait (this takes time!)'
CALL PGPT(m, x(:m), y(:m), -1)

CALL PGEND

END SUBROUTINE
