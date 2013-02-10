PROGRAM StrangelyAttractive
IMPLICIT NONE

! Limits for equation coefficients
REAL, PARAMETER :: a_min = -3.
REAL, PARAMETER :: a_max = 3.

! Draw loops?
LOGICAL, PARAMETER :: draw_loops = .FALSE.

! Consider unbound if bigger than this
INTEGER, PARAMETER :: unbound_limit = 1000000
! Iterate that many generations to go to attractor
INTEGER, PARAMETER :: init_iterations = 1000
! Skip that many generations before starting to calculate Lyapunov exponents
INTEGER, PARAMETER :: skip_iterations = 50
! Total number of iterations to calculate
INTEGER, PARAMETER :: total_iterations = 2000000
! The last iteration to draw
INTEGER, PARAMETER :: last_draw_iteration = 20000
! Check at this step if boring
INTEGER, PARAMETER :: boring_check = 10000
! Consider Lyapunov exponent negative, if smaller than this
REAL, PARAMETER :: l_epsilon = -1E-4
! Start at this point
REAL, PARAMETER :: x0 = 0.1
REAL, PARAMETER :: y0 = 0.1

! Equation coefficients
REAL, DIMENSION(12) :: a
! Current point
REAL x, y
! Calculated points
REAL, DIMENSION(total_iterations) :: points_x
REAL, DIMENSION(total_iterations) :: points_y

! Lyaupunov exponents stuff
! Tangent space basis
REAL, DIMENSION(2) :: e1, e2
! Temprorary variables
REAL :: n1, n2
REAL :: l_acc1, l_acc2
! Fixing factor for logarithm
REAL :: log2 = LOG(2.)
! 'Boring check' total steps
INTEGER, PARAMETER :: check = &
                      boring_check - init_iterations - skip_iterations
! Total steps for Lyapunov exponents calculation
INTEGER, PARAMETER :: l_total = &
                      total_iterations - init_iterations - skip_iterations
! Exponents themselves
REAL :: l_exp1, l_exp2

INTEGER :: i, n
LOGICAL :: again = .TRUE.
LOGICAL :: err = .FALSE.

! Quadratic map
INTERFACE
    SUBROUTINE QUADRATIC_STEP(a, x, y)
        REAL, DIMENSION(12), INTENT(IN) :: a
        REAL, INTENT(INOUT) :: x, y
    END SUBROUTINE QUADRATIC_STEP
END INTERFACE
! And its linearization
INTERFACE
    SUBROUTINE LINEAR_STEP(a, x, y, e1, e2, n1, n2)
        REAL, DIMENSION(12), INTENT(IN) :: a
        REAL, INTENT(IN) :: x, y
        REAL, DIMENSION(2), INTENT(INOUT) :: e1, e2
        REAL, INTENT(OUT) :: n1, n2
    END SUBROUTINE LINEAR_STEP
END INTERFACE

! Drawing
INTERFACE
    FUNCTION DRAW(m, x, y)
    LOGICAL DRAW
    INTEGER, INTENT(IN) :: m
    REAL, DIMENSION(:), INTENT(IN) :: x, y
    END FUNCTION DRAW
END INTERFACE

CALL RANDOM_SEED()

main_loop: DO WHILE (again)
    ! Initialize coefficients
    CALL RANDOM_NUMBER(a)
    a = a_max * a + a_min * (1. - a)

    ! Initialize starting points
    x = x0
    y = y0

    ! Initialize Lyapunov exponents stuff
    l_acc1 = 0
    l_acc2 = 0
    e1 = (/ 1., 0. /)
    e2 = (/ 0., 1. /)

    err = .FALSE.

    iterations: DO n = 1, total_iterations

        ! Iterate tangent space basis, if needed
        IF (n > init_iterations) THEN
            CALL LINEAR_STEP(a, x, y, e1, e2, n1, n2)
            ! Record exponents if needed
            sum: IF (n > init_iterations + skip_iterations) THEN
                l_acc1 = l_acc1 + LOG(n1)
                l_acc2 = l_acc2 + LOG(n2)
            END IF sum
        END IF

        ! Step to the next point
        CALL QUADRATIC_STEP(a, x, y)
        points_x(n) = x
        points_y(n) = y

        ! Check if unbound
        IF (MAX(ABS(x), ABS(y)) > unbound_limit) THEN
            err = .TRUE.
            EXIT iterations
        END IF

        ! Check if boring
        IF (n == boring_check) THEN
            l_exp1 = l_acc1 / check / log2
            l_exp2 = l_acc2 / check / log2
            boring: IF (MAX(l_exp1, l_exp2) < l_epsilon) THEN
                WRITE (*, '(A)', ADVANCE='NO') 'Current Lyapunov exponents:'
                WRITE (*, '(1X, F5.2, 1X, F5.2)'), l_exp1, l_exp2
                WRITE (*, '(A)') 'Boring.'
                err = .TRUE.
                EXIT iterations
            END IF boring
        END IF

    END DO iterations

    IF (err) THEN
        CYCLE main_loop
    END IF

    WRITE (*, '(A)') '*****************************************************'
    WRITE (*, '(A)') 'Parameters used:'
    WRITE (*, '(4X 6(F6.2))') ( a(i), i=1,6 )
    WRITE (*, '(4X 6(F6.2))') ( a(i), i=7,12 )
    l_exp1 = l_acc1 / l_total / log2
    l_exp2 = l_acc2 / l_total / log2
    WRITE (*, '(A)', ADVANCE='NO') 'Lyapunov exponents:'
    WRITE (*, '(1X, F5.2, A, 1X, F5.2, A)') l_exp1, ' bits/iteration', &
                                            l_exp2, ' bits/iteration'

    IF (l_exp1 < l_epsilon) THEN
        WRITE(*, '(A)') 'Boring.'
        CYCLE main_loop
    END IF

    loop_or_chaos: IF (ABS(l_exp1) < ABS(l_epsilon)) THEN
        skip_loop: IF (draw_loops) THEN
            WRITE(*, '(A)') 'Loop'
        ELSE
            WRITE(*, '(A)') 'Skipping loop.'
            CYCLE main_loop
        END IF skip_loop
    ELSE
        WRITE(*, '(A)', ADVANCE='NO') 'Chaos. Dimension:'
        full_2d: IF (l_exp2 > 0) THEN
            WRITE(*, '(I1)') 2
        ELSE
            WRITE(*, '(F5.2)') 1. + l_exp1/ABS(l_exp2)
        ENDIF full_2d
    ENDIF loop_or_chaos
    WRITE (*, '(A)') '*****************************************************'

    ! We found it. Go to visualization.
    again = DRAW(last_draw_iteration - init_iterations, &
                 points_x(init_iterations+1:), &
                 points_y(init_iterations+1:))

END DO main_loop

CONTAINS

FUNCTION RUN_AGAIN()
LOGICAL RUN_AGAIN
CHARACTER(1) :: s
WRITE (*, '(A)', ADVANCE='NO') "Try again [Y/N]? "
READ (*, *) s
IF ((s == 'Y') .OR. (s == 'y')) THEN
    RUN_AGAIN = .TRUE.
ELSE
    RUN_AGAIN = .FALSE.
END IF
END FUNCTION

END PROGRAM StrangelyAttractive
