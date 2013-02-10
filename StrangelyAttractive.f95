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
! Total number of iterations
INTEGER, PARAMETER :: total_iterations = 20000
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
! Jacobian
REAL, DIMENSION(2, 2) :: jacobian
! Tangent space basis
REAL, DIMENSION(2) :: e1, e2
! Temprorary variables
REAL, DIMENSION(2) :: e2turned
REAL :: n1, n2
REAL :: l_acc1, l_acc2
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

INTERFACE
    SUBROUTINE DRAW(n, x, y)
    INTEGER, INTENT(IN) :: n
    REAL, DIMENSION(n), INTENT(IN) :: x, y
    END SUBROUTINE
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
            jacobian = J(a, x, y)
            e1 = MATMUL(jacobian, e1)
            e2 = MATMUL(jacobian, e2)
            ! Now perform reorthonormalization
            n1 = NORM2(e1)
            e1 = e1 / n1
            e2turned = e2 - DOT_PRODUCT(e2, e1) * e1
            n2 = NORM2(e2turned)
            e2 = e2turned / n2
            ! Record exponents if needed
            sum: IF (n > init_iterations + skip_iterations) THEN
                l_acc1 = l_acc1 + LOG(n1)
                l_acc2 = l_acc2 + LOG(n2)
            END IF sum
        END IF

        ! Step to the next point
        CALL ITERATION(a, x, y)
        points_x(n) = x
        points_y(n) = y

        ! Check if unbound
        IF (MAX(ABS(x), ABS(y)) > unbound_limit) THEN
            err = .TRUE.
            EXIT iterations
        END IF

        ! Check if boring
        IF (n == boring_check) THEN
            l_exp1 = l_acc1 / check
            l_exp2 = l_acc2 / check
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
    l_exp1 = l_acc1 / l_total
    l_exp2 = l_acc2 / l_total
    WRITE (*, '(A)', ADVANCE='NO') 'Lyapunov exponents:'
    WRITE (*, '(1X, F5.2, 1X, F5.2)'), l_exp1, l_exp2

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
    CALL DRAW(total_iterations - init_iterations, &
              points_x(init_iterations+1:), &
              points_y(init_iterations+1:))

    ! Do it again?
    again = RUN_AGAIN()

END DO main_loop


CONTAINS

! Built-in in Fortran 2008
FUNCTION NORM2(V)
REAL :: NORM2
REAL, DIMENSION(2), INTENT(IN) :: V
NORM2 = SQRT(V(1)**2 + V(2)**2)
END FUNCTION NORM2

FUNCTION J(a, x, y)
REAL, DIMENSION(2, 2) :: J
REAL, DIMENSION(12), INTENT(IN) :: a
REAL, INTENT(IN) :: x, y

REAL :: dXx, dXy, dYx, dYy

dXx = a(2) + a(4) * y + 2 * a(5) * x
dXy = a(3) + a(4) * x + 2 * a(6) * y
dYx = a(8) + a(10) * y + 2 * a(11) * x
dYy = a(9) + a(10) * x + 2 * a(12) * y

J(1,1) = dXx
J(1,2) = dXy
J(2,1) = dYx
J(2,2) = dYy

END FUNCTION J

SUBROUTINE ITERATION(a, x, y)
REAL, DIMENSION(12), INTENT(in) :: a
REAL, INTENT(inout) :: x, y
REAL :: tmp, xy, x2, y2

xy = x * y
x2 = x ** 2
y2 = y ** 2

tmp = a(1) + a(2) * x + a(3) * y + a(4) * xy + a(5) * x2 + a(6) * y2
y = a(7) + a(8) * x + a(9) * y + a(10) * xy + a(11) * x2 + a(12) * y2
x = tmp

END SUBROUTINE ITERATION

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
