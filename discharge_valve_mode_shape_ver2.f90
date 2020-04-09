subroutine discharge_valve_mode_shape_ver2(t_dv, w_dv_x2, w_dv_x3, l_dvef, r_dv_x1, l_dv_x2, l_dv_x3, dia_port, F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start, phi_mode_mid, phi_mode_end, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back)

    ! --------------------------------------------------------
    ! Calculate mode shape parameter
    ! According to Prof. Ooi 1992 paper
    ! --------------------------------------------------------
    implicit none
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    integer n, i, switch_valve, j
    common/valve_case/switch_valve
    double precision area_effective_force, t_dv, w_dv_x2, w_dv_x3, l_dvef, r_dv_x1, l_dv_x2, l_dv_x3, dia_port       ! DV dimension input
    double precision F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, F_mode_squared, F_mode_squared_back, Fc_mass_valve, phi_mode_start, phi_mode_mid, phi_mode_end, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back     ! DV parameters
    double precision x1, x2, x3, x, x_mid, x_end, h_x1, h_x2, h_x3, y_up, y_down_1, y_down_2, y_down_3, f_down_1, f_down_2, f_down_3, A_cross_down_1, A_cross_down_2, A_cross_down_3 ! calculation dummy variables
    double precision y_mode_squared_1, y_mode_squared_back_1, y_mode_squared_2, y_mode_squared_back_2, f_mode_squared_1, f_mode_squared_back_1, f_mode_squared_2, f_mode_squared_back_2, f_mass_1, y_mass_1
    double precision y_up_back, y_down_1_back, y_down_2_back, y_down_3_back, f_down_1_back, f_down_2_back, f_down_3_back            ! dummy variables for circumstances hitting the back plate
    common/normal_disc_valve/F_mode_squared, F_mode_squared_back, Fc_mass_valve
    ! ------------- Rectangular valve part ----------------
    double precision x_rec
    double precision w_dv_rec, l_dv_rec, l_dv_rec_ef
    common/rec_discharge_valve/w_dv_rec, l_dv_rec, l_dv_rec_ef
    double precision h_x_rec, y_up_rec, y_up_back_rec, y_down_rec, y_down_back_rec, f_down_rec, f_down_back_rec, f_mode_squared_rec, f_mode_squared_rec_back, y_mode_squared_rec, y_mode_squared_rec_back
    double precision F_up_dv_rec, F_up_dv_back_rec, F_down_dv_rec, F_down_dv_back_rec, phi_mode_start_rec, phi_mode_mid_rec, phi_mode_start_back_rec, phi_mode_mid_back_rec, phi_mode_end_rec, phi_mode_end_back_rec, A_cross_rec, Fc_mode_squared_rec, Fc_mode_squared_rec_back, Fc_mass_dv_rec
    common/rec_disc_valve_F/F_up_dv_rec, F_up_dv_back_rec, F_down_dv_rec, F_down_dv_back_rec, phi_mode_start_rec, phi_mode_mid_rec, phi_mode_start_back_rec, phi_mode_mid_back_rec, phi_mode_end_rec, phi_mode_end_back_rec, A_cross_rec, Fc_mode_squared_rec, Fc_mode_squared_rec_back, Fc_mass_dv_rec

    
    area_effective_force = 0.25*pi*(dia_port*1.d-3)**2      ! estimated effective force area of discharge pressure force
    
    ! -------------------------
    ! initialize the parameter
    ! integration is done using Weddle's Rule
    ! F_xxxxx = force term
    ! -------------------------
    F_up_dv = 0.
    F_down_dv = 0.  
    
    ! ---------------------------
    ! Direction of "x", origin at the port, not the screw
    ! i.e. port is located at x = 0 to dia_port or 2*r_dv_x1
    ! x2 is the second section of the reed valve
    ! x3 is the third section (screw part)
    ! h_x1~h_x3 is the increment based on weddle's rule
    ! ---------------------------
    
    x = 0.0         
    x_mid = x + r_dv_x1*1.d-3
    x_end = x + 2.*(r_dv_x1 - 0.77)*1.d-3
    !h = (l_dvef - x_hole_start)*1.d-3/6.
    x1 = 0.0
    x2 = l_dv_x2*1.d-3
    !x3 = l_dv_x3*1.d-3  ! w/o part 3
    !h_x1 = 2*r_dv_x1*1.d-3/6.
    !h_x1 = ((2*r_dv_x1 - 0.77)*1.d-3)/6.      ! corrected part 1 step 
    h_x1 = (l_dv_x2*1.d-3)/1296.      ! corrected part 1 step 
    h_x2 = ((l_dv_x3 - l_dv_x2)*1.d-3)/1296.
    !h_x3 = (l_dvef - l_dv_x3)*1.d-3/6. ! w/o part 3
    
    !y_up = 0.
    y_down_1 = 0.
    y_down_2 = 0.
    !y_down_3 = 0. ! w/o part 3
    
    y_down_1_back = 0.
    y_down_2_back = 0. 
    !y_down_3_back = 0. ! w/o part 3
    
    y_mode_squared_1 = 0.
    y_mode_squared_2 = 0.
    y_mode_squared_back_1 = 0.
    y_mode_squared_back_2 = 0.
     
    y_mass_1 = 0.
    
    !phi_mode_start = ((x/(l_dvef*1.d-3))**4 - 4.0*(x/(l_dvef*1.d-3)) + 3.0)*1.d-3
    !phi_mode_mid = ((x_mid/(l_dvef*1.d-3))**4 - 4.0*(x_mid/(l_dvef*1.d-3)) + 3.0)*1.d-3
    !phi_mode_end = ((x_end/(l_dvef*1.d-3))**4 - 4.0*(x_end/(l_dvef*1.d-3)) + 3.0)*1.d-3
    !
    !!call dv_hit_back(l_dvef, r_dv_x1, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back)
    !phi_mode_start_back = ((x/(l_dvef*1.d-3))**4 - (1.5)*(x/(l_dvef*1.d-3))**3 + (0.5)*(x/(l_dvef*1.d-3)))*1.d-3
    !phi_mode_mid_back = ((x_mid/(l_dvef*1.d-3))**4 - (1.5)*(x_mid/(l_dvef*1.d-3))**3 + (0.5)*(x_mid/(l_dvef*1.d-3)))*1.d-3
    !phi_mode_end_back = ((x_end/(l_dvef*1.d-3))**4 - (1.5)*(x_end/(l_dvef*1.d-3))**3 + (0.5)*(x_end/(l_dvef*1.d-3)))*1.d-3
    
    phi_mode_start = ((x/(l_dvef*1.d-3))**4 - 4.0*(x/(l_dvef*1.d-3)) + 3.0)
    phi_mode_mid = ((x_mid/(l_dvef*1.d-3))**4 - 4.0*(x_mid/(l_dvef*1.d-3)) + 3.0)
    phi_mode_end = ((x_end/(l_dvef*1.d-3))**4 - 4.0*(x_end/(l_dvef*1.d-3)) + 3.0) 
    
    !call dv_hit_back(l_dvef, r_dv_x1, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back)
    phi_mode_start_back = ((x/(l_dvef*1.d-3))**4 - (1.5)*(x/(l_dvef*1.d-3))**3 + (0.5)*(x/(l_dvef*1.d-3)))
    phi_mode_mid_back = ((x_mid/(l_dvef*1.d-3))**4 - (1.5)*(x_mid/(l_dvef*1.d-3))**3 + (0.5)*(x_mid/(l_dvef*1.d-3)))
    phi_mode_end_back = ((x_end/(l_dvef*1.d-3))**4 - (1.5)*(x_end/(l_dvef*1.d-3))**3 + (0.5)*(x_end/(l_dvef*1.d-3)))    
    
    ! ------------------------------------------ 
    ! For constant cross-sectional area valve (rectangular valve)
    ! Mode shape is found from FEM by K.R. Heng (2018)
    ! ------------------------------------------
    if (switch_valve == 2) then
        x_rec = 0.0
        h_x_rec = l_dv_rec_ef*1.d-3/6.0
        A_cross_rec = w_dv_rec * t_dv * 1.d-6
        phi_mode_start_rec = -0.000999651808676*(l_dv_rec_ef*1.d-3-x) +    6.281583461338063*(l_dv_rec_ef*1.d-3-x)**2 -89.442914101700040*(l_dv_rec_ef*1.d-3-x)**3 +408.300248005862784*(l_dv_rec_ef*1.d-3-x)**4
        phi_mode_mid_rec = -0.000999651808676*(l_dv_rec_ef*1.d-3-x_mid) +    6.281583461338063*(l_dv_rec_ef*1.d-3-x_mid)**2 -89.442914101700040*(l_dv_rec_ef*1.d-3-x_mid)**3 +  408.300248005862784*(l_dv_rec_ef*1.d-3-x_mid)**4
        phi_mode_end_rec = -0.000999651808676*(l_dv_rec_ef*1.d-3-x_end) +    6.281583461338063*(l_dv_rec_ef*1.d-3-x_end)**2 -89.442914101700040*(l_dv_rec_ef*1.d-3-x_end)**3 +  408.300248005862784*(l_dv_rec_ef*1.d-3-x_end)**4
        phi_mode_start_back_rec = 0.0000129287*(l_dv_rec_ef*1.d-3-x) +0.9061999706*(l_dv_rec_ef*1.d-3-x)**2 -29.7779824125*(l_dv_rec_ef*1.d-3-x)**3 -163.4611444823*(l_dv_rec_ef*1.d-3-x)**4 + 8655.9566055533*(l_dv_rec_ef*1.d-3-x)**5 +47441.1065061872*(l_dv_rec_ef*1.d-3-x)**6 -1410521.4437184029*(l_dv_rec_ef*1.d-3-x)**7
        phi_mode_mid_back_rec = 0.0000129287*(l_dv_rec_ef*1.d-3-x_mid) +0.9061999706*(l_dv_rec_ef*1.d-3-x_mid)**2 -29.7779824125*(l_dv_rec_ef*1.d-3-x_mid)**3 -163.4611444823*(l_dv_rec_ef*1.d-3-x_mid)**4 + 8655.9566055533*(l_dv_rec_ef*1.d-3-x_mid)**5 +47441.1065061872*(l_dv_rec_ef*1.d-3-x_mid)**6 -1410521.4437184029*(l_dv_rec_ef*1.d-3-x_mid)**7
        phi_mode_end_back_rec = 0.0000129287*(l_dv_rec_ef*1.d-3-x_end) +0.9061999706*(l_dv_rec_ef*1.d-3-x_end)**2 -29.7779824125*(l_dv_rec_ef*1.d-3-x_end)**3 -163.4611444823*(l_dv_rec_ef*1.d-3-x_end)**4 + 8655.9566055533*(l_dv_rec_ef*1.d-3-x_mid)**5 +47441.1065061872*(l_dv_rec_ef*1.d-3-x_mid)**6 -1410521.4437184029*(l_dv_rec_ef*1.d-3-x_mid)**7
        F_up_dv_rec = 0.
        F_down_dv_rec = 0. 
        y_down_rec = 0.
        y_down_back_rec = 0.
        f_down_rec = 0.
        f_down_back_rec = 0.
        y_mode_squared_rec = 0.
        y_mode_squared_rec_back = 0.
        F_mode_squared_rec = 0.
        F_mode_squared_rec_back = 0.
    endif
    ! --------------------
    ! constant cross-sectional area at section 2 and 3
    ! x-sectional area of section have to be integrated
    ! --------------------
    A_cross_down_2 = w_dv_x2 * t_dv * 1.d-6
    !A_cross_down_3 = w_dv_x3 * t_dv * 1.d-6     ! w/o part 3
do j = 1,216
    do 374 i = 1,7
        
        !y_up = area_effective_force*((x/(l_dvef*1.d-3))**4 - (1.5)*(x/(l_dvef*1.d-3))**3 + (0.5)*(x/(l_dvef*1.d-3)))
        !y_down = rho_dv*2.*(t_dv*1.d-3)*(2.*(r_dv_x3*1.d-3)*(x - x_hole_start*1.d-3) - (x - x_hole_start*1.d-3)**2)**(0.5) * ((x/(l_dvef*1.d-3))**4 - (1.5)*(x/(l_dvef*1.d-3))**3 + (0.5)*(x/(l_dvef*1.d-3)))**2
        A_cross_down_1 = 2.*(t_dv*1.d-3)*(2.*(r_dv_x1*1.d-3)*x1 - x1**2)**(0.5)
        if (2.*(r_dv_x1*1.d-3)*x1 - x1**2 < 0) then
            A_cross_down_1 = 0.0                ! to counter the roundup problem of ForTran
        endif

    ! --------------------------------
    ! Prof Ooi 1992 paper
    ! equation 2.5, shape function phi_mode at each point is calculated in "discharge_valve_parameters_ver2.f90"
    ! here calculate the right hand side of Eq. 2.5, 
    ! and integration of the shape function square
    ! --------------------------------
        !y_up   = area_effective_force*(1*1.d-3*(x1/(l_dvef*1.d-3))**4 - 4.0*1.d-3*(x1/(l_dvef*1.d-3)) + 3.0*1.d-3)
        !f_down_1 = A_cross_down_1*((1*(x1/(l_dvef*1.d-3))**4 - 4.0*(x1/(l_dvef*1.d-3)) + 3.0)*1.d-3)**2
        !f_down_2 = A_cross_down_2*((1*(x2/(l_dvef*1.d-3))**4 - 4.0*(x2/(l_dvef*1.d-3)) + 3.0)*1.d-3)**2
        !f_down_3 = A_cross_down_3*((1*(x3/(l_dvef*1.d-3))**4 - 4.0*(x3/(l_dvef*1.d-3)) + 3.0)*1.d-3)**2
        !
        !f_down_1_back = A_cross_down_1*((1*(x1/(l_dvef*1.d-3))**4 - 1.5*(x1/(l_dvef*1.d-3))**3 + (0.5)*(x1/(l_dvef*1.d-3)))*1.d-3)**2
        !f_down_2_back = A_cross_down_2*((1*(x2/(l_dvef*1.d-3))**4 - 1.5*(x2/(l_dvef*1.d-3))**3 + (0.5)*(x2/(l_dvef*1.d-3)))*1.d-3)**2
        !f_down_3_back = A_cross_down_3*((1*(x3/(l_dvef*1.d-3))**4 - 1.5*(x3/(l_dvef*1.d-3))**3 + (0.5)*(x3/(l_dvef*1.d-3)))*1.d-3)**2
        
        f_down_1 = A_cross_down_1*((1.0*(x1/(l_dvef*1.d-3))**4 - 4.0*(x1/(l_dvef*1.d-3)) + 3.0))**2      ! Squared mode shape
        f_down_2 = A_cross_down_2*((1.0*(x2/(l_dvef*1.d-3))**4 - 4.0*(x2/(l_dvef*1.d-3)) + 3.0))**2      ! Squared mode shape
        !f_down_3 = A_cross_down_3*((1*(x3/(l_dvef*1.d-3))**4 - 4.0*(x3/(l_dvef*1.d-3)) + 3.0))**2   ! w/o part 3
        
        !f_down_1 = A_cross_down_1*((1*(x1/(l_dvef*1.d-3))**4 - 4.0*(x1/(l_dvef*1.d-3)) + 3.0))
        !f_down_2 = A_cross_down_2*((1*(x2/(l_dvef*1.d-3))**4 - 4.0*(x2/(l_dvef*1.d-3)) + 3.0))
        
        f_down_1_back = A_cross_down_1*(1.0*(x1/(l_dvef*1.d-3))**4 - 1.5*(x1/(l_dvef*1.d-3))**3 + (0.5)*(x1/(l_dvef*1.d-3)))**2        ! Squared mode shape
        f_down_2_back = A_cross_down_2*(1.0*(x2/(l_dvef*1.d-3))**4 - 1.5*(x2/(l_dvef*1.d-3))**3 + (0.5)*(x2/(l_dvef*1.d-3)))**2        ! Squared mode shape
        !f_down_3_back = A_cross_down_3*((1*(x3/(l_dvef*1.d-3))**4 - 1.5*(x3/(l_dvef*1.d-3))**3 + (0.5)*(x3/(l_dvef*1.d-3))))**2     ! w/o part 3
        
        !f_down_1_back = A_cross_down_1*((1*(x1/(l_dvef*1.d-3))**4 - 1.5*(x1/(l_dvef*1.d-3))**3 + (0.5)*(x1/(l_dvef*1.d-3))))
        !f_down_2_back = A_cross_down_2*((1*(x2/(l_dvef*1.d-3))**4 - 1.5*(x2/(l_dvef*1.d-3))**3 + (0.5)*(x2/(l_dvef*1.d-3))))
        
        f_mode_squared_1 = ((1.0*(x1/(l_dvef*1.d-3))**4 - 4.0*(x1/(l_dvef*1.d-3)) + 3.0))**2
        f_mode_squared_2 = ((1.0*(x2/(l_dvef*1.d-3))**4 - 4.0*(x2/(l_dvef*1.d-3)) + 3.0))**2
        f_mode_squared_back_1 = ((1.0*(x1/(l_dvef*1.d-3))**4 - 1.5*(x1/(l_dvef*1.d-3))**3 + (0.5)*(x1/(l_dvef*1.d-3))))**2
        f_mode_squared_back_2 = ((1.0*(x2/(l_dvef*1.d-3))**4 - 1.5*(x2/(l_dvef*1.d-3))**3 + (0.5)*(x2/(l_dvef*1.d-3))))**2
        
        f_mass_1 = A_cross_down_1
    ! ---------------------------------------
    ! Rectangular valve reed
    ! ---------------------------------------
        f_down_rec = A_cross_rec*(-0.000999651808676*(x_rec) +    6.281583461338063*(x_rec)**2 -89.442914101700040*(x_rec)**3 +408.300248005862784*(x_rec)**4)**2      ! commented out, no squared
        f_down_back_rec = A_cross_rec*(0.0000129287*(x_rec) +0.9061999706*(x_rec)**2 -29.7779824125*(x_rec)**3 -163.4611444823*(x_rec)**4 + 8655.9566055533*(x_rec)**5 +47441.1065061872*(x_rec)**6 -1410521.4437184029*(x_rec)**7)**2     ! commented out, no squared
        !f_down_rec = A_cross_rec*(-0.000999651808676*(x_rec) +    6.281583461338063*(x_rec)**2 -89.442914101700040*(x_rec)**3 +408.300248005862784*(x_rec)**4)
        !f_down_back_rec = A_cross_rec*(0.0000129287*(x_rec) +0.9061999706*(x_rec)**2 -29.7779824125*(x_rec)**3 -163.4611444823*(x_rec)**4 + 8655.9566055533*(x_rec)**5 +47441.1065061872*(x_rec)**6 -1410521.4437184029*(x_rec)**7)
        
        f_mode_squared_rec = (-0.000999651808676*(x_rec) +    6.281583461338063*(x_rec)**2 -89.442914101700040*(x_rec)**3 +408.300248005862784*(x_rec)**4)**2
        f_mode_squared_rec_back = (0.0000129287*(x_rec) +0.9061999706*(x_rec)**2 -29.7779824125*(x_rec)**3 -163.4611444823*(x_rec)**4 + 8655.9566055533*(x_rec)**5 +47441.1065061872*(x_rec)**6 -1410521.4437184029*(x_rec)**7)**2
        
        if ( i == 1 .or. i == 3 .or. i == 5 .or. i == 7) then
            n = 1
        elseif (i == 2 .or. i == 6) then
            n = 5
        elseif (i == 4) then
            n = 6
        endif
        
        !F_up_dv   = F_up_dv + 0.3*h*n*y_up            ! Numerator of force term
        !F_down_dv = F_down_dv + 0.3*h*n*y_down      ! Denominator of force term
        y_down_1 = y_down_1 + 0.3*h_x1*n*f_down_1
        y_down_2 = y_down_2 + 0.3*h_x2*n*f_down_2
        !y_down_3 = y_down_3 + 0.3*h_x3*n*f_down_3   ! w/o part 3
        
        y_down_1_back = y_down_1_back + 0.3*h_x1*n*f_down_1_back
        y_down_2_back = y_down_2_back + 0.3*h_x2*n*f_down_2_back
        !y_down_3_back = y_down_3_back + 0.3*h_x3*n*f_down_3_back    ! w/o part 3
        
        ! --- square of mode shape
        y_mode_squared_1 = y_mode_squared_1 + 0.3*h_x1*n*f_mode_squared_1
        y_mode_squared_2 = y_mode_squared_2 + 0.3*h_x2*n*f_mode_squared_2
        y_mode_squared_back_1 = y_mode_squared_back_1 + 0.3*h_x1*n*f_mode_squared_back_1
        y_mode_squared_back_2 = y_mode_squared_back_2 + 0.3*h_x2*n*f_mode_squared_back_2
        
        y_down_rec = y_down_rec + 0.3*h_x_rec*n*f_down_rec
        y_down_back_rec = y_down_back_rec + 0.3*h_x_rec*n*f_down_back_rec
        y_mode_squared_rec = y_mode_squared_rec + 0.3*h_x_rec*n*f_mode_squared_rec
        y_mode_squared_rec_back = y_mode_squared_rec_back + 0.3*h_x_rec*n*f_mode_squared_rec_back
        
        y_mass_1 = y_mass_1 + 0.3*h_x1*n*f_mass_1
        
        x1 = x1 + h_x1
        x2 = x2 + h_x2
        !x3 = x3 + h_x3  ! w/o part 3
        
        x_rec = x_rec + h_x_rec
        
374 continue
        enddo
        y_up = phi_mode_mid*area_effective_force
        y_up_back = phi_mode_mid_back*area_effective_force
        
        F_up_dv = y_up          ! will be appear again in discharge_valve_deflection_ver2.f90
        F_up_dv_back = y_up_back
        
        !F_down_dv = rho_dv*(y_down_1 + y_down_2 + y_down_3)
        !F_down_dv_back = rho_dv*(y_down_1_back + y_down_2_back + y_down_3_back)
        F_down_dv = rho_dv*(y_down_1 + y_down_2)    ! w/o part 3
        F_down_dv_back = rho_dv*(y_down_1_back + y_down_2_back) ! w/o part 3
        
        F_mode_squared = (y_mode_squared_1 + y_mode_squared_2)
        F_mode_squared_back = (y_mode_squared_back_1 + y_mode_squared_back_2)
        
        Fc_mass_valve = rho_dv*(A_cross_down_2*(l_dv_x3 - l_dv_x2)*1.d-3 + y_mass_1)

        ! ---- Rectangular -----
        y_up_rec = phi_mode_mid_rec*area_effective_force    ! rectangular
        y_up_back_rec = phi_mode_mid_back_rec*area_effective_force  ! rectangular valve when hit the back
        F_up_dv_rec = y_up_rec
        F_up_dv_back_rec = y_up_back_rec
        F_down_dv_rec = rho_dv*y_down_rec
        F_down_dv_back_rec = rho_dv*y_down_back_rec
        Fc_mode_squared_rec = y_mode_squared_rec
        Fc_mode_squared_rec_back = y_mode_squared_rec_back
        Fc_mass_dv_rec = rho_dv*A_cross_rec*l_dv_rec_ef*1.d-3

endsubroutine