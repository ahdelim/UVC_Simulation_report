subroutine discharge_valve_deflection_ver2(area_port_open, y_mid_check, y_end_check, y_mid, y_start, F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start, phi_mode_mid, phi_mode_end, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back, omega_natural_dv, omega_natural_dv_back, rho_dv, dpratio_dv, p_dcv, p_disc, g_r, h_r, ystartn, ymidn, yendn, y_stop, gn, hn)
    
    implicit none
    include "var_physical_constants.f90"
    integer switch_valve
    common/valve_case/switch_valve
    double precision F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, F_mode_squared, F_mode_squared_back, Fc_mass_valve, phi_mode_start, phi_mode_mid, phi_mode_end, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back ! force num and den, mode shapes
    double precision omega_natural_dv, omega_natural_dv_back, rho_dv, dpratio_dv, p_dcv, p_disc, y_stop
    double precision coef_dv, coef_dv_back
    common/valve_para_2/coef_dv, coef_dv_back
    double precision sur_tension_oil, angle_oil_film, ratio_val_port
    common/valve_para_3/sur_tension_oil, angle_oil_film, ratio_val_port
    common/normal_disc_valve/F_mode_squared, F_mode_squared_back, Fc_mass_valve
    double precision g_r, h_r, gn, hn, ystartn, ymidn, yendn         ! g = q, h = dq/dt
    double precision y_mid, y_start, y_mid_check, y_end_check          ! decision making variables
    double precision area_port_open
    double precision dia_suc, dia_disc, length_suc, length_disc                 ! Ports Diameters & Port Length
    common/port_size_var/dia_suc, dia_disc, length_suc, length_disc      ! common block for PORT
    double precision F_vis, F_ten
    ! ---------------- Rectangular ----------------------
    double precision omega_natural_dv_rec, omega_natural_dv_back_rec
    double precision F_up_dv_rec, F_up_dv_back_rec, F_down_dv_rec, F_down_dv_back_rec, phi_mode_start_rec, phi_mode_mid_rec, phi_mode_start_back_rec, phi_mode_mid_back_rec, phi_mode_end_rec, phi_mode_end_back_rec, A_cross_rec, Fc_mode_squared_rec, Fc_mode_squared_rec_back, Fc_mass_dv_rec
    common/rec_disc_valve_F/F_up_dv_rec, F_up_dv_back_rec, F_down_dv_rec, F_down_dv_back_rec, phi_mode_start_rec, phi_mode_mid_rec, phi_mode_start_back_rec, phi_mode_mid_back_rec, phi_mode_end_rec, phi_mode_end_back_rec, A_cross_rec, Fc_mode_squared_rec, Fc_mode_squared_rec_back, Fc_mass_dv_rec
    common/rec_valve_nat_freq/omega_natural_dv_rec, omega_natural_dv_back_rec
    ! ---------------------------------------------------
    
    

    if (y_start < y_stop*1.d-3 .or. (y_start == y_stop*1.d-3 .and. h_r < 0.0)) then
        ! ---------------------------------------------------
        ! Calculate force term due to oil stiction effect (Khalifa and Liu, 1998)
        ! F_vis = viscous force term
        ! F_ten = surface tension force term
        ! ---------------------------------------------------
        !F_vis = 0.5*3.0*pi*miu_oil*(0.5*dia_disc**1.d-3)**4*(1.0 - ratio_val_port**4 + (1.0 - 2.0*ratio_val_port**2 + ratio_val_port**4)/log(ratio_val_port))*phi_mode_mid
        F_vis = 0.0
        !F_ten = pi*(0.5*dia_disc**1.d-3)**2*(ratio_val_port**2 - 1.0)*(2.0*sur_tension_oil*cos(angle_oil_film))
        F_ten = 0.0

        ! ---------------------------------------------------
        ! 1st Case (Case 1)
        ! when the valve just leave the seat and before hitting the stop plate (Cantilever clamped-free)
        ! Diff. Equation as follows:
        ! dgdt = h        ! dq/dt in Prof Ooi's paper or dphi2/dt = phi1 in K.M.Tan thesis
        ! dhdt = (p_dcv - p_disc)*1000.0*F_up_dv/F_down_dv - 2.0*dpratio_dv*omega_natural_dv*h - ((omega_natural_dv)**2)*g  ! dphi1/dt in K.M.Tan thesis
        ! Deflection y = psi*g   (phi*g) or (phi*q)
        ! ---------------------------------------------------
        ! update the upper term here
        ! based on the changing port opening (effective force area)
        ! to add a coefficient which obtained from experiment
        ! --------------------------
            F_up_dv = coef_dv*phi_mode_mid*area_port_open       ! Force when the valve just leave the seat before hitting the back plate
            F_up_dv_back = coef_dv_back*phi_mode_mid_back*area_port_open     ! Force when the valve hit the back plate

        ! ---------------------------------------------------
        ! Rectangular valve 
        ! ---------------------------------------------------
            F_up_dv_rec = coef_dv*phi_mode_mid_rec*area_port_open
            F_up_dv_back_rec = coef_dv_back*phi_mode_mid_back_rec*area_port_open
        !if (y_start < y_stop*1.d-3 .or. (y_start == y_stop*1.d-3 .and. h_r .le. 0.0)) then   
            !!call Runge_valve(g_r, h_r, f_mid, integral_f, p_dcv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, g_grav, omega_response_dv, gn, hn)
        if (switch_valve == 2) then
            call Runge_valve_ver2(omega_natural_dv_rec, F_vis, F_ten, y_mid, F_up_dv_rec, F_down_dv_rec, Fc_mode_squared_rec, Fc_mass_dv_rec, rho_dv, dpratio_dv, p_dcv, p_disc, g_r, h_r, gn, hn)
        
            ystartn = phi_mode_start_rec*gn
            yendn = phi_mode_end_rec*gn
            ymidn = phi_mode_mid_rec*gn
        
        
        elseif (switch_valve == 1) then     ! normal valve
            call Runge_valve_ver2(omega_natural_dv, F_vis, F_ten, y_mid, F_up_dv, F_down_dv, F_mode_squared, Fc_mass_valve, rho_dv, dpratio_dv, p_dcv, p_disc, g_r, h_r, gn, hn)
            !call RK4_gnhn(g_r, h_r, time_step, f_mid, p_dcv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, omega_response_dv, gn, hn)
        
            ystartn = phi_mode_start*gn
            yendn = phi_mode_end*gn
            ymidn = phi_mode_mid*gn
        endif
        
        
            if (ystartn < 0.0) then         ! Reed Valve is modelled as cantilever beam (clamped-free), may have -ve deflection, which means hitting the valve seat
                ystartn = 0.0
                gn = 0.0
                hn = 0.0
            elseif (ystartn == 0.0 .and. hn .le. 0.0) then
                ystartn = 0.0
                gn = 0.0
                hn = 0.0
            endif
        
            if (ymidn < 0.0) then  
                ymidn = 0.0
                gn = 0.0
                hn = 0.0
            elseif (ymidn == 0.0 .and. hn .le. 0.0) then
                ymidn = 0.0
                gn = 0.0
                hn = 0.0
            endif
        
            if (yendn < 0.0) then  
                yendn = 0.0
                gn = 0.0
                hn = 0.0
            elseif (yendn == 0.0 .and. hn .le. 0.0) then
                yendn = 0.0
                gn = 0.0
                hn = 0.0
            endif
        !endif
    
        ! ---------------------------------------------------
        ! 2nd Case (Clamped-pinned support)
        ! Check if valve hit the valve stop
        ! Different mode shape function
        ! ---------------------------------------------------
    elseif (y_start .ge. y_stop*1.d-3 .and. h_r .ge. 0.0) then      ! Reed Valve hit the valve stop with an upward velocity
        F_vis = 0.0
        !F_ten = pi*(0.5*dia_disc**1.d-3)**2*(ratio_val_port**2 - 1.0)*(2.0*sur_tension_oil*cos(angle_oil_film))
        F_ten = 0.0
            g_r = (y_mid - y_mid_check)/phi_mode_mid_back 
            !h_r = phi_mode_mid/phi_mode_mid_back * h_r
            if (switch_valve == 2) then
                g_r = (y_mid - y_mid_check)/phi_mode_mid_back_rec 
                call Runge_valve_ver2(omega_natural_dv_back_rec, F_vis, F_ten, y_mid, F_up_dv_back_rec, F_down_dv_back_rec, Fc_mode_squared_rec_back, Fc_mass_dv_rec, rho_dv, dpratio_dv, p_dcv, p_disc, g_r, h_r, gn, hn)
                
                ystartn = y_stop*1.d-3
                ymidn = phi_mode_mid_back_rec*gn + y_mid_check
                yendn = phi_mode_end_back_rec*gn + y_end_check
                
            elseif (switch_valve == 1) then
                call Runge_valve_ver2(omega_natural_dv_back, F_vis, F_ten, y_mid, F_up_dv_back, F_down_dv_back, F_mode_squared_back, Fc_mass_valve, rho_dv, dpratio_dv, p_dcv, p_disc, g_r, h_r, gn, hn)
                
                ystartn = y_stop*1.d-3
                ymidn = phi_mode_mid_back*gn + y_mid_check
                yendn = phi_mode_end_back*gn + y_end_check
            endif

            ! ------------------------
            ! set gn w.r.t case 1 using ymidn and phi_mode_mid (normal case, before hitting stop plate)
            ! because next iteration may occur in case 1
            ! everytime run for case 1 first, if only the condition here is satisfied, then go to case 2 
            ! 
            ! ------------------------
            !gn = ymidn/phi_mode_mid
            if (switch_valve == 2) then
                gn = ymidn/phi_mode_mid_rec
            elseif (switch_valve == 1) then
                gn = ymidn/phi_mode_mid
            endif
           ! hn = phi_mode_mid_back/phi_mode_mid * hn
    endif
    
    if (ystartn .ge. y_stop*1.d-3) then
        ystartn = y_stop*1.d-3
    endif
    if (ymidn .ge. y_stop*1.d-3) then
        ymidn = y_stop*1.d-3
    endif
    if(yendn .ge. y_stop*1.d-3) then
        yendn = y_stop*1.d-3
    endif
    
endsubroutine