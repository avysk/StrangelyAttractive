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

    LOGICAL :: do_init, reset_limits, again
    LOGICAL :: do_box = .TRUE.

    m = m0
    n = SIZE(x)
    do_init = .TRUE.
    reset_limits = .TRUE.
    again = .TRUE.

    drawing: DO WHILE (again)

        setup_limits: IF (reset_limits) THEN
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
            reset_limits = .FALSE.
        END IF setup_limits

        ! Init pgplot library
        IF (do_init) THEN
            init: BLOCK
                INTEGER :: ier
                INTERFACE PGBEG
                    FUNCTION PGBEG(ignored, device, nxsub, nysub)
                        INTEGER :: PGBEG
                        INTEGER, INTENT(IN) :: ignored
                        CHARACTER*(*), INTENT(IN) :: device
                        INTEGER, INTENT(IN) :: nxsub, nysub
                    END FUNCTION PGBEG
                END INTERFACE PGBEG
                ier = PGBEG(0, '?', 1, 1)
                if (ier /= 1) STOP 'Cannot open output device'
                do_init = .FALSE.
            END BLOCK init
        END IF


        CALL PGBBUF

        CALL PGENV(xmin, xmax, ymin, ymax, 0, 0)

        ! We want black on white
        CALL PGSCR(0, 1., 1., 1.)
        CALL PGSCR(1, 0., 0., 0.)
        CALL PGERAS

        coordinates: IF (do_box) THEN
            ! We just erased coordinate box; redraw it
            CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0) 
        END IF coordinates

        WRITE (*, '(A)') 'Drawing, please wait (this takes time!)'
        CALL PGPT(m, x(:m), y(:m), -1)

        CALL PGEBUF

        WRITE (*, '(I10, A)') m, ' points plotted'
        WRITE (*, '(A)') 'F: [F]ind next map (default)'
        WRITE (*, '(A)') 'N: Change the [N]umber of points to plot'
        WRITE (*, '(A)') 'D: Select different drawing [D]evice'
        WRITE (*, '(A)') 'L: Change X/Y [L]imits'
        WRITE (*, '(A)') 'R: [R]eset X/Y limits'
        WRITE (*, '(A)') 'S: [S]witch coordinates on/off'
        WRITE (*, '(A)') 'Q: [Q]uit'
        WRITE (*, '(A)', ADVANCE='NO') '=> '

        prompt: BLOCK
            CHARACTER(LEN=1) :: reply

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
                    CALL PGCLOS
                CASE ('q', 'Q')
                    DRAW = .FALSE.
                    again = .FALSE.
                CASE ('l', 'L')
                    WRITE (*, '(A)', ADVANCE='NO') 'Xmin: '
                    READ (*, *) xmin
                    WRITE (*, '(A)', ADVANCE='NO') 'Xmax: '
                    READ (*, *) xmax
                    WRITE (*, '(A)', ADVANCE='NO') 'Ymin: '
                    READ (*, *) ymin
                    WRITE (*, '(A)', ADVANCE='NO') 'Ymax: '
                    READ (*, *) ymax
                CASE ('r', 'R')
                    reset_limits = .TRUE.
                CASE ('s', 'S')
                    do_box = .NOT. do_box
                CASE DEFAULT
                    DRAW = .TRUE.
                    again = .FALSE.
            END SELECT
        END BLOCK prompt

    END DO drawing

    CALL PGEND

END FUNCTION DRAW
