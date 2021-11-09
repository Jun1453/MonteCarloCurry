program demo_mc
    use ellipsoid
    implicit none
    integer(8) :: l, num_iter=2000
    real(8) :: x, y, z=0, k1, k2, k3, k4, count=0, sum=0
    real(8) :: t=10, h, perm, visc, satu=1.05, Q
    real(8), external :: flux, dist
    open(10, file="demo_mc.out")
    open(11, file="demo_mc.ext")
    open(12, file="demo_mc.int")

    h = 5e-2

    ! viscosity and permeability are fixed in the demo
    visc = 10
    perm = 10. ** (-1)

    ! note parameters down
    write(10,"(3(A,F4.1))") "# Ellipsoid axes: a=", a, ", b=", b, ", c=", c
    write(10,"(2(A,F7.3))") "# Viscosity:", visc, ", Permeability:", perm
    
    90 continue
        call random_number(x)
        call random_number(y)
        ! call random_number(z) ! fixed z for elliptic profile

        ! refer to the main program
        if (((x/a)**2 + (y/b)**2 + (z/c)**2) .LT. 1.) then
            Q = 0
            do l = 1, int(t/h)
                k1 = flux(Q     , perm, satu, visc, x, y, z) * h
                k2 = flux(Q+k1/2, perm, satu, visc, x, y, z) * h
                k3 = flux(Q+k2/2, perm, satu, visc, x, y, z) * h
                k4 = flux(Q+k3  , perm, satu, visc, x, y, z) * h
                Q = Q + (k1 + 2*k2 + 2*k3 + k4) / 6.
            end do
            if ( Q/satu .LT. 1. .AND. Q/satu .GT. 0 ) then 
                sum = sum + Q/satu
                count = count + 1.
                ! for the colorized points in demo figure
                ! output to .out and .int files
                write(10,*) x, y, Q/satu
                write(12,*) x, y, Q/satu
            end if
        else
            ! for the gray points in demo figure
            ! output to .out and .ext files
            write(10,*) x, y, -1
            write(11,*) x, y, -1
        end if

    ! to mark down the division for animation frame
    if ( mod(int(count),200) .EQ. 0 ) then
        write(11,*) "####"
        write(12,*) "####"
    end if

    if ( count .LT. num_iter ) go to 90

    ! summary output
    write(10,"(A,F7.2,A,F22.15)") "# t=", t, " s, Ratio:", sum/count

end program