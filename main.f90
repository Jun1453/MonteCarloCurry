program main
    implicit none
    integer(8) :: i, j, k, num_iter=10000
    real(8) :: i_min=0.1, j_min=1.2, i_max=10.1, j_max=1.7, delta_i=0.5, delta_j=0.05
    real(8) :: a=1., b=1., c=1., x, y, z, chi, dist, area, count=0, sum=0, mean
    real(8) :: t=10, perm=1e-3, visc, satu
    ! open(10, filename="ratio.out")

    do i = 0, int((i_max-i_min)/delta_i)
        visc = i_min+i*delta_i
        do j = 0, int((j_max-j_min)/delta_j)
            satu = j_min+j*delta_j
            do k = 1, num_iter
                call random_number(x)
                call random_number(y)
                call random_number(z)
                if (((x/a)**2 + (y/b)**2 + (z/c)**2) .LT. 1.) then
                    ! write(*,*) i_min+i*delta_i, j_min+j*delta_j
                    dist = 1. - sqrt(x**2+y**2+z**2)
                    area = 4./3. * acos(-1.) * (x*y + y*z + x*z)
                    chi = 1. - exp(-perm*area*t/visc/dist)/satu
                    ! chi = satu - exp(-perm*area*t/visc/dist) 
                    sum = sum + chi
                    count = count + 1.
                else 
                    ! k = k -1
                end if
            end do
            mean = sum/count
            write(10,*) visc, satu, mean
            count = 0
            sum = 0
        end do
    end do

end program