module ellipsoid
    implicit none
    ! ! japonica (short-grain rice)
    ! real(8), parameter :: a = 0.6
    ! real(8), parameter :: b = 0.4
    ! real(8), parameter :: c = 0.4

    ! indica (long-grain rice)
    real(8), parameter :: a = 1.0
    real(8), parameter :: b = 0.3
    real(8), parameter :: c = 0.3

    contains
    ! Calculate the distance from an inner point to the surface of the rice grain
    ! Distance from a point to an ellipsoid: https://www.ma.ic.ac.uk/~rn/distance2ellipse.pdf
    ! Newton's method for multivariable case: https://azrael.digipen.edu/MAT180/NewtonsMethod.pdf
    real(8) function dist_point_ellipsoid(x, y, z)
        implicit none
        real(8), intent(IN) :: x, y, z
        real(8) :: r=1, theta, phi, a11, a12, a21, a22, det, f1=1., f2=1., epsilon=1e-5
        integer(8) :: count=0

        theta = atan2(a*y, b*x)
        phi = atan2(z, c*sqrt((x/a)**2+(y/b)**2))

        ! find the closest point on the surface using Jacobian matrix
        do
            f1 = (a**2-b**2)*r*cos(theta)*sin(theta)*cos(phi) - x*a*sin(theta) + y*b*cos(theta)
            f2 = (a**2*cos(theta)**2 + b**2*sin(theta)**2 - c**2)*r*sin(phi)*cos(phi) &
               - x*a*sin(phi)*cos(theta) - y*b*sin(phi)*sin(theta) + z*c*cos(phi)
            if ( (f1 .LT. epsilon .AND. f2 .LT. epsilon) .OR. count .GT. 1000 ) exit
            a11 = (a**2-b**2)*r*(cos(theta)**2-sin(theta)**2)*cos(phi) - x*a*cos(theta) - y*b*sin(theta)
            a12 = -(a**2-b**2)*r*cos(theta)*sin(theta)*sin(phi)
            a21 = -2*(a**2-b**2)*r*cos(theta)*sin(theta)*sin(phi)*cos(phi) &
                + x*a*sin(phi)*sin(theta) - y*b*sin(phi)*cos(theta)
            a22 = (a**2*cos(theta)**2+b**2*sin(theta)**2-c**2)*r*(cos(phi)**2-sin(phi)**2) &
                - x*a*cos(phi)*cos(theta) - y*b*cos(phi)*sin(theta) - z*c*sin(phi)
            det = a11*a22 - a12*a21

            theta = theta - ( a22*f1 - a12*f2) / det
            phi   = phi   - (-a21*f1 + a11*f2) / det
            count = count + 1
        end do

        ! return the distance between two points
        dist_point_ellipsoid = sqrt((r*a*cos(phi)*cos(theta)-x)**2 + (r*b*cos(phi)*sin(theta)-y)**2 + (r*c*sin(phi)-z)**2)
        return
    end function

end module ellipsoid