subroutine geo_revolution_theta_2(theta_2, theta_1, l_v, r_oc, r_ro, r_vh, e)
    
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"
    double precision, dimension (1:no_data+1) :: theta_2, theta_1, l_v
    double precision r_oc, r_ro, r_vh, e
    double precision dummy_cos
    integer i
    
    do 300 i = 1, no_data+1
        dummy_cos = (((r_oc + r_vh)**2 - e**2 - (r_ro + l_v(i) + r_vh)**2)/(2.*e*(r_ro + l_v(i) + r_vh)))
        if (abs(dummy_cos) > 1.0) then
            dummy_cos = 1.0
        !elseif (abs(dummy_cos) >1.0 .and. i == no_data+1) then
           ! dummy_cos = 1.0
        endif
        
        if (theta_1(i)*180.0/pi < 179.999980d0) then
            theta_2(i) = acos(dummy_cos)
        elseif (theta_1(i)*180.0/pi <= 180.000020d0 .and. theta_1(i)*180.0/pi >= 179.999980d0) then
            theta_2(i) = pi
        elseif (theta_1(i)*180.0/pi > 180.000020d0) then
            theta_2(i) = 2*pi - acos(dummy_cos)
        endif

        !if (abs(((r_oc + r_vh)**2 - e**2 - (r_ro + l_v(i) + r_vh)**2)/(2.*e*(r_ro + l_v(i) + r_vh))) > 1.0 .and. i == 1) then
        !    theta_2(i) = 0.0
        !elseif (abs(((r_oc + r_vh)**2 - e**2 - (r_ro + l_v(i) + r_vh)**2)/(2.*e*(r_ro + l_v(i) + r_vh))) > 1.0 .and. i == no_data+1) then
        !    theta_2(i) = 360.0
        !endif
        !
300 continue
        !theta_2((no_data+1)/2) = pi
end subroutine geo_revolution_theta_2