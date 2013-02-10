PURE SUBROUTINE QUADRATIC_STEP(a, x, y)
IMPLICIT NONE
! Inputs:
! a -- 12 coefficients for quadratic map
! x, y -- current point
!
! Outputs:
! x, y -- the point after one iteration
    REAL, DIMENSION(12), INTENT(IN) :: a
    REAL, INTENT(INOUT) :: x, y
    REAL :: tmp, xy, x2, y2

    xy = x * y
    x2 = x ** 2
    y2 = y ** 2

    tmp = a(1) + a(2) * x + a(3) * y + a(4) * xy + a(5) * x2 + a(6) * y2
    y = a(7) + a(8) * x + a(9) * y + a(10) * xy + a(11) * x2 + a(12) * y2
    x = tmp

END SUBROUTINE QUADRATIC_STEP

PURE SUBROUTINE LINEAR_STEP(a, x, y, e1, e2, n1, n2)
IMPLICIT NONE
! Inputs:
! a -- 12 coefficients for quadratic map
! x, y -- current point
! e1, e2 -- current tangent space orthonormal basis
!
! Ouptuts:
! e1, e2 -- the result of orthonormalization of Jacobian applied to e1 and e2
! n1, n2 -- norms of (new) e1 and e2 before normalization
    REAL, DIMENSION(12), INTENT(IN) :: a
    REAL, INTENT(IN) :: x, y
    REAL, DIMENSION(2), INTENT(INOUT) :: e1, e2
    REAL, INTENT(OUT) :: n1, n2

    REAL, DIMENSION(2, 2) :: jacobian
    REAL, DIMENSION(2) :: e2_rotated

    jacobian = J(a, x, y)
    e1 = MATMUL(jacobian, e1)
    e2 = MATMUL(jacobian, e2)
    ! Reorthonormalize
    n1 = NORM2(e1)
    e1 = e1 / n1
    e2_rotated = e2 - DOT_PRODUCT(e2, e1) * e1
    n2 = NORM2(e2_rotated)
    e2 = e2_rotated / n2

    CONTAINS

    ! Jacobian
    PURE FUNCTION J(a, x, y)
        IMPLICIT NONE
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

END SUBROUTINE LINEAR_STEP
