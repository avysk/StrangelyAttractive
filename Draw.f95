FUNCTION DRAW(m0, x, y)
IMPLICIT NONE

    LOGICAL :: DRAW

    INTEGER, INTENT(IN) :: m0
    REAL, DIMENSION(:), INTENT(IN) :: x, y

    ! How many data ponits we have
    INTEGER :: n
    ! How many data points we want to draw
    INTEGER :: m
    ! add that amount of space around the drawing
    REAL, PARAMETER :: space = 0.05

    REAL :: xmin, xmax, ymin, ymax, deltax, deltay

    INTEGER :: ier, PGBEG

    LOGICAL :: do_init, again
    CHARACTER(LEN=1) :: reply

    m = m0
    n = SIZE(x)
    do_init = .TRUE.
    again = .TRUE.

    drawing: DO WHILE (again)

        ! Init pgplot library
        init: IF (do_init) THEN
            ier = PGBEG(0, '?', 1, 1)
            if (ier /= 1) STOP
            do_init = .FALSE.
        END IF init

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

        WRITE (*, '(I10, A)') m, ' points plotted'
        WRITE(*, '(A)') '[F]ind next map (default)'
        WRITE(*, '(A)') 'Change the [N]umber of points to plot'
        WRITE(*, '(A)') 'Select different drawing [D]evice'
        WRITE(*, '(A)') '[Q]uit'
        WRITE (*, '(A)', ADVANCE='NO') '=> '
        READ (*, *) reply

        SELECT CASE (reply)
            CASE ('n', 'N')
                WRITE (*, '(I10, A)', ADVANCE='NO') n, &
                      ' data points available, how many to plot? '
                READ (*, *) m
                sanity: IF (m > n) THEN
                    m = n
                END IF sanity
                WRITE (*, '(I10, A)') m, ' poins will be drawn'
            CASE ('d', 'D')
                do_init = .TRUE.
            CASE ('q', 'Q')
                DRAW = .FALSE.
                again = .FALSE.
            CASE DEFAULT
                DRAW = .TRUE.
                again = .FALSE.
        END SELECT

    END DO drawing

    CALL PGEND

END FUNCTION DRAW
