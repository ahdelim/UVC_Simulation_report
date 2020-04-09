subroutine geo_vanelength(l_v, theta_1, r_oc, r_ro, r_vh, e)
    include "var_simulation_parameter.f90"
    double precision, dimension (1:no_data+1) :: l_v, theta_1
    double precision r_oc, r_ro, r_vh, e, l_v_dead
    integer i
    include "var_physical_constants.f90"
    l_v_dead = 1.d-11
    do 210 i = 1, no_data+1
        
        ! ----------------------------
        ! NVC Vane length
        ! ----------------------------
        l_v(i) = - r_ro - r_vh + sqrt(e**2 + (r_oc + r_vh)**2 - 2*e*(r_oc + r_vh)*cos(theta_1(i)))
        ! ---------------------------
        ! MCRC 
        ! ---------------------------
        !l_v1(i) = sqrt(e*e + r_oc**2 - 2*e*r_oc*cos(theta_1(i)))- r_ih - t_h
        ! l_v2(i) = sqrt(r_ih*r_ih - (r_ih - r_ic)*(r_ih - r_ic)*sin(theta_1(i))*sin(theta_1(i))) - r_ic - (r_ih - r_ic)*cos(theta_1(i))        
        !l_v2(i) = sqrt((r_ih-0.5*w_vane2)**2 - e*e*sin(theta_1(i))**2) - r_ic - e*cos(theta_1(i)) + 0.5*w_vane2
      
210 continue
    
    do i = 1, no_data + 1
        if (l_v(i) < 0.0) then
            l_v(i) = 0.0
        endif
    enddo
!2985 format (11A15)
!2986 format (F15.4, 10ES15.6)  
end subroutine geo_vanelength