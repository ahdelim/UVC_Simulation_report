subroutine geo_revolution_theta_1(theta_1)
    
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"
    double precision, dimension (1:no_data+1) :: theta_1   ! Revolution angle
    integer :: i
    theta_1(1) = theta_start*pi/180.0     ! start point of theta_1
    
    do 200 i = 1, no_data
        theta_1(i+1) = theta_1(i) + theta_step
200 continue
       
end subroutine geo_revolution_theta_1
    
