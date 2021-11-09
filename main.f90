program main
    use ellipsoid
    implicit none
    integer(8) :: i, j, k, l, num_iter=50
    real(8) :: i_min=0.1, j_min=-4, i_max=10.1, j_max=-0.95, delta_i=0.1, delta_j=0.05
    real(8) :: x, y, z, k1, k2, k3, k4, count=0, sum=0, mean
    real(8) :: t=10, h, perm, visc, satu=1.05, Q
    real(8), external :: flux
    open(10, file="10s.out")

    ! time interval
    h = 5e-2

    ! loop along x-axis: viscosity
    do i = 0, int((i_max-i_min)/delta_i)
        visc = i_min+i*delta_i

        ! loop along y-axis: permeability
        do j = 0, int((j_max-j_min)/delta_j)
            perm = 10. ** (j_min+j*delta_j)

            ! monte-carlo iteration
            ! equalvalent: do k = 1, num_iter
            90 continue
                ! randomize the location of point in ([0,1], [0,1])
                call random_number(x)
                call random_number(y)
                call random_number(z)

                ! if the point is in the ellipsoid
                if (((x/a)**2 + (y/b)**2 + (z/c)**2) .LT. 1.) then
                    ! 4th order Runge-Ketta
                    Q = 0
                    do l = 1, int(t/h)
                        k1 = flux(Q     , perm, satu, visc, x, y, z) * h
                        k2 = flux(Q+k1/2, perm, satu, visc, x, y, z) * h
                        k3 = flux(Q+k2/2, perm, satu, visc, x, y, z) * h
                        k4 = flux(Q+k3  , perm, satu, visc, x, y, z) * h
                        Q = Q + (k1 + 2*k2 + 2*k3 + k4) / 6.
                    end do 
                    ! only put reasonable chi into the average
                    ! in case that all sampling points are outside (rare)
                    if ( Q/satu .LT. 1. .AND. Q/satu .GT. 0 ) then 
                        sum = sum + Q/satu
                        count = count + 1.
                    end if
                end if

            ! end do
            if (count .LT. num_iter) go to 90

            ! calculate the average (chi-bar)
            mean = sum/count

            ! variant for plotting
            ! write(10,*) visc, satu, mean
            write(10,*) visc, (j_min+j*delta_j), mean

            ! reset the counting varaibles
            count = 0.
            sum = 0.

        end do

        ! empty line for gnuplot `splot` command
        write(10,*)

    end do

end program