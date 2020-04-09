subroutine dv_hit_back(l_dvef, r_dv_x1, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back)
    ! ------------------------------------------
    ! When reed valve hit the back plate
    ! Calculate mode shape (phi)
    ! ------------------------------------------
    double precision l_dvef, r_dv_x1
    double precision phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back
    double precision x, x_mid, x_end
    
    x = 0.0
    x_mid = x + r_dv_x1*1.d-3
    x_end = x + 2.*r_dv_x1*1.d-3
    
    phi_mode_start_back = ((x/(l_dvef*1.d-3))**4 - (1.5)*(x/(l_dvef*1.d-3))**3 + (0.5)*(x/(l_dvef*1.d-3)))*1.d-3
    phi_mode_mid_back = ((x_mid/(l_dvef*1.d-3))**4 - (1.5)*(x_mid/(l_dvef*1.d-3))**3 + (0.5)*(x_mid/(l_dvef*1.d-3)))*1.d-3
    phi_mode_end_back = ((x_end/(l_dvef*1.d-3))**4 - (1.5)*(x_end/(l_dvef*1.d-3))**3 + (0.5)*(x_end/(l_dvef*1.d-3)))*1.d-3
    
endsubroutine dv_hit_back