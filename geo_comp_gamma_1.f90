subroutine geo_comp_gamma_1(gamma_1, l_v, theta_1, r_oc, r_vh, r_ro, e)
    
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"
    double precision, dimension (1:no_data+1) :: gamma_1, l_v, theta_1         ! gamma_1 is the subtended angle (refer to report)
    double precision r_oc, r_vh, r_ro, e
    
    do 470 i = 1, no_data+1
        
        if (theta_1(i)*180.0/pi < 180.0) then
            gamma_1(i) = - acos(((r_oc + r_vh)**2 + (r_ro + l_v(i) + r_vh)**2 - e**2)/(2*(r_oc + r_vh)*(r_ro + l_v(i) + r_vh)))
        elseif (theta_1(i)*180.0/pi == 180.0) then
            gamma_1(i) = 0.0
        else
            gamma_1(i) = + acos(((r_oc + r_vh)**2 + (r_ro + l_v(i) + r_vh)**2 - e**2)/(2*(r_oc + r_vh)*(r_ro + l_v(i) + r_vh)))
        endif
        if (abs(((r_oc + r_vh)**2 + (r_ro + l_v(i) + r_vh)**2 - e**2)/(2*(r_oc + r_vh)*(r_ro + l_v(i) + r_vh))) > 1.0) then
            gamma_1(i) = 0.0
        endif
470 continue
        
end subroutine geo_comp_gamma_1