PROGRAM StrangelyAttractive
    IMPLICIT NONE
    INTEGER i, ier, pgbeg
    INTEGER c1, c2
    REAL xr(100), yr(100)
    REAL xs(5), ys(5)
    DATA xs/1.,2.,3.,4.,5./
    DATA ys/1.,4.,9.,16.,25./
    ier = pgbeg(0, '/AQT', 1, 1)
    IF (ier.NE.1) STOP
    CALL pgqcol(c1, c2)
    WRITE (*, *) c1
    WRITE (*, *) c2
    CALL pgenv(0.,20.,0.,100.,0,-2)
    CALL pgscr(0, 1., 1., 1.)
    CALL pgscr(1, 0., 0., 0.)
!    CALL PGLAB('(x)', '(y)', 'A simple graph')
    CALL pgpt(5, xs, ys, 9)
    DO 10 i=1,100
        xr(i) = 0.1*i
        yr(i) = xr(i)**2
10  CONTINUE
!    CALL PGSCI(2)
    CALL pgscr(1, 1., 0., 0.)
!    CALL PGLINE(100,XR,YR)
    CALL pgpt(100, xr, yr, -1)
    CALL pgend
end program StrangelyAttractive
