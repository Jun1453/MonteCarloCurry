real(8) function flux(Q, k, S, mu, x, y, z)
    use ellipsoid
    implicit none
    real(8), intent(IN) :: Q, k, S, mu, x, y, z
    real(8) :: d

    ! calculate flux (dQ/dt) using darcy's law
    d = dist_point_ellipsoid(x, y, z)
    flux = k * area(x, y, z) * (S - Q) / mu / d
    return

    contains
    real(8) function area(x, y, z)
        implicit none
        real(8), intent(IN) :: x, y, z
        real(8), parameter :: p = 1.6075
        real(8) :: r_sq
        
        r_sq = (x/a)**2 + (y/b)**2 + (z/c)**2
        ! approximate surface area of a ellpsoid
        area = 4. * acos(-1.) * (r_sq*((a*b)**p + (b*c)**p + (a*c)**p)/3)**(1/p)
        return
    end function area

end function flux


real(8) function dist(x, y, z)
use ellipsoid
implicit none
real(8), intent(IN) :: x, y, z

dist = dist_point_ellipsoid(x, y, z)
return
end function dist