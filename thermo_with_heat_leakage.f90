subroutine thermo_with_heat_leakage(theta_1, v_com, v_suc, dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_di, dmdtheta1_do, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dmdtheta1_s, dedtheta1_s, dmdtheta1_d, dedtheta1_d, dvdtheta1_s, dvdtheta1_d, y_start, y_mid, y_end)
    implicit none
    !   -----------------------------------
    !   Note:
    !   Dead volume is considered.
    !   ------------------------------------
    ! Simulation Parameter
    include "var_simulation_parameter.f90"
    
    ! coefficient and operational parameter
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    
    ! Valve parameters
    include "var_reed_valve.f90"
    
    ! Suction and Discharge Condition variables
    include "var_operating_fluid_condition.f90"
    
    ! Compressor main dimensions
    include "var_main_dimensions.f90"
    
    ! Geometrical Variables
    include "var_geometrical.f90"
    include "var_geometrical_diff.f90"
    include "var_clearance.f90"
    
    ! Thermodynamic Variables
    include "var_thermo.f90"
    
    ! Compressor Performance
    include "var_compressor_evaluation.f90"
    
    ! ------ Discharge valve (rectangular) --------- !
    !double precision, dimension (1:no_data+1) :: omega_response_dv_1                                                                ! for rectangular valve
    !double precision f_start_1, f_mid_1, f_end_1, omega_natural_dv_1, A_cross_dv_1, I_dv_1, beta_r_1, integral_f_1                  ! for rectangular valve
    
    ! ------ Heat Transfer Input Variables --------- !
    double precision, dimension (1:no_data+1) :: dqdtheta_sc, dqdtheta_dc
    ! ------ Leakage Input variables ----------- !
    double precision, dimension (1:no_data+1) :: dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d
    
    double precision, dimension (1:no_data+1) :: g_r, h_r, y_start, y_mid, y_end                                           ! for ver2, g = q, h = dq/dt
    double precision, dimension (1:no_data+1) :: y_mid_check, y_end_check
    
    double precision area_port_open ! discharge port opening area
    double precision m_1_dummy, h_1_dummy ! dummy variables for residual mass case
    integer i ,k

    ! ----------------------------------------------------------------------------- !
    !  Suction Process
    ! ----------------------------------------------------------------------------- !
    ! Initilization of variables
    ! ----------------------------------------------------------------------------- !
    
    p_scv(1) = p_suc ![kPa]
    t_scv(1) = t_suc ![K]
    u_scv(1) = u_suc ![j/kg]
    rho_scv(1) = rho_suc ![kg/m3]
    h_scv(1) = h_suc ![j/kg]
    s_scv(1) = s_suc ![J/kg.K]
    miu_scv(1) = miu_suc ![Pa.s (Pa.s) or N.s/m^2]
    cv_scv(1) = cv_suc  ! [J/kg.K]
    cp_scv(1) = cp_suc ![J/kg.K]
    k_scv(1) = k_suc !W/(m.K)
    m_scv(1) = rho_scv(1) * v_suc(1)*1.d-9![kg]
    
    ! --------------------------------------------------------
    ! Thermodynamic Equation
    ! --------------------------------------------------------
    do 271 i = 1, no_data+1
        if (p_suc > p_scv(i)) then
            !call m_flow_suc(omega_1, coef_sucport, p_scv(i), rho_suc, s_suc, h_suc, dia_suc, dmdtheta1_si(i))         ! Mass Flow Model for suction
            call m_flow_suc_ver2(r_oc, theta_1(i), omega_1, coef_sucport, p_scv(i), rho_suc, s_suc, h_suc, dia_suc, dmdtheta1_si(i), theta_suc_start, theta_suc_end)
            dmdtheta1_so(i) = 0.0
        else
            dmdtheta1_si(i) = 0.0
            !call m_flow_suc(omega_1, coef_sucport, p_suc, rho_scv(i), s_scv(i), h_scv(i), dia_suc, dmdtheta1_so(i))
            call m_flow_suc_ver2(r_oc, theta_1(i), omega_1, coef_sucport, p_suc, rho_scv(i), s_scv(i), h_scv(i), dia_suc, dmdtheta1_so(i), theta_suc_start, theta_suc_end)
        endif
        
        dqdtheta1_s(i) = dqdtheta_sc(i)              ! Assume adiabatic
        dmdtheta1_s(i) = dmdtheta1_si(i) - dmdtheta1_so(i) + dmdtheta1_leak_s(i)      ! [kg/rad]
        dedtheta1_s(i) = dmdtheta1_si(i)*h_suc - dmdtheta1_so(i)*h_scv(i) + dedtheta1_leak_s(i)    ! [J/rad]
        !dvdtheta1_s(i) = dvdtheta1_suc(i)*1.d-9        ! [m^3/rad]
        
        !if (i == 1) then
        !   dvdtheta1_s(i) = 0      ! [m3/rad]
        !elseif (i == no_data+1) then
        !   dvdtheta1_s(i) = ((v_suc(i) - v_suc(i-1))/(theta_1(i)-theta_1(i-1)))*1.d-9
        !else
        !   dvdtheta1_s(i) = ((v_suc(i+1) - v_suc(i))/(theta_1(i+1)-theta_1(i)))*1.d-9       ! [m3/rad]
        !endif
        
        if (i == no_data+1) then
           dvdtheta1_s(i) = ((v_suc(i) - v_suc(i-1))/(theta_1(i)-theta_1(i-1)))*1.d-9
        else
           dvdtheta1_s(i) = ((v_suc(i+1) - v_suc(i))/(theta_1(i+1)-theta_1(i)))*1.d-9       ! [m3/rad]
        endif
        
        
        call next_prop_cv(dqdtheta1_s(i), dmdtheta1_s(i), dedtheta1_s(i), p_scv(i), dvdtheta1_s(i), m_scv(i), u_scv(i), v_suc(i+1), u_scv(i+1), m_scv(i+1), rho_scv(i+1), p_scv(i+1), t_scv(i+1), h_scv(i+1), s_scv(i+1), miu_scv(i+1), cv_scv(i+1), cp_scv(i+1), k_scv(i+1), v_suc(i))

        
271 continue
    
    ! ----------------------------------------------------------------------------- 
    !  Discharge Process 
    ! ----------------------------------------------------------------------------- 

    p_dcv(1) = p_scv(no_data+1)         ![kPa]
    t_dcv(1) = t_scv(no_data+1)         ![K]
    u_dcv(1) = u_scv(no_data+1)         ![j/kg]
    rho_dcv(1) = rho_scv(no_data+1)     ![kg/m3]
    h_dcv(1) = h_scv(no_data+1)         ![j/kg]
    s_dcv(1) = s_scv(no_data+1)         ![J/kg.K]
    miu_dcv(1) = miu_scv(no_data+1)     ![Pa.s (Pa.s) or N.s/m^2]
    cv_dcv(1) = cv_scv(no_data+1)       ! [J/kg.K]
    cp_dcv(1) = cp_scv(no_data+1)       ![J/kg.K]
    k_dcv(1) = k_scv(no_data+1)         !W/(m.K)
    m_dcv(1) = m_scv(no_data+1)         ![kg]
    !m_dcv(1) = 2.78757D-05

    
    ! --------------------------------------------------------
    ! Residual mass re-expanded to suction (being compress again)
    ! m_dcv*h_dcv = m_1*h_1 + m_residual*h_residual
    ! m_dcv = m_1 + m_residual
    ! --------------------------------------------------------
    
    m_1_dummy = m_dcv(1)
    h_1_dummy = h_dcv(1)
    m_dcv(1) = m_1_dummy + mass_residual_dead
    h_dcv(1) = (m_1_dummy*h_1_dummy + mass_residual_dead*h_residual_dead)/m_dcv(1)
    rho_dcv(1) = m_dcv(1) / (v_com(1)*1.d-9)
    call refprop_rho_h(rho_dcv(1), h_dcv(1), t_dcv(1), p_dcv(1), u_dcv(1), s_dcv(1), cp_dcv(1), miu_dcv(1), k_dcv(1))
    
    g_r(1) = 0.0        ! initial modal displacement
    h_r(1) = 0.0        ! initial modal velocity (h = dg/dt)
    y_start(1) = 0.0
    y_mid(1) = 0.0
    y_end(1) = 0.0
    
    k = 1
    do 277 i = 1, no_data!+1
    ! -----------------------------------------------
    ! return flow, when residual gas pump in and pressure
    ! is larger than the outside suction pressure
    ! -----------------------------------------------
    if (theta_1(i) <= theta_suc_end .and. p_dcv(i) > p_suc) then
        dmdtheta1_di(i) = 0.0
        call m_flow_suc_ver2(r_oc, theta_1(i), omega_1, coef_sucport, p_suc, rho_dcv(i), s_dcv(i), h_dcv(i), dia_suc, dmdtheta1_do(i), theta_suc_start, theta_suc_end)
    else
        if (p_dcv(i) > p_disc) then
            dmdtheta1_di(i) = 0.0
            !call m_flow_disc(omega_1, coef_discport_1, y_start_1(i), y_end_1(i), p_disc, s_dcv_1(i), h_dcv_1(i), dia_disc1, dmdtheta1_do_1(i))
            !call m_flow_disc_ver2(omega_1, coef_discport, y_mid(i), p_disc, rho_dcv(i), s_dcv(i), h_dcv(i), dia_disc, dmdtheta1_do(i))
            call m_flow_disc_ver2(r_oc, theta_1(i), omega_1, coef_discport, y_mid(i), p_disc, rho_dcv(i), s_dcv(i), h_dcv(i), dia_disc, dmdtheta1_do(i), theta_disc_start, theta_disc_end, area_port_open)
        else
            !call m_flow_disc(omega_1, coef_discport_1, y_start_1(i), y_end_1(i), p_dcv_1(i), s_disc, h_disc, dia_disc1, dmdtheta1_di_1(i))
            !call m_flow_disc_ver2(omega_1, coef_discport, y_mid(i), p_dcv(i), rho_disc, s_disc, h_disc, dia_disc, dmdtheta1_di(i))
            call m_flow_disc_ver2(r_oc, theta_1(i), omega_1, coef_discport, y_mid(i), p_dcv(i), rho_disc, s_disc, h_disc, dia_disc, dmdtheta1_di(i), theta_disc_start, theta_disc_end, area_port_open)
            dmdtheta1_do(i) = 0.0
        endif
    endif
    
        dqdtheta1_d(i) = dqdtheta_dc(i)            ! Heat transfer
        dmdtheta1_d(i) = dmdtheta1_di(i) - dmdtheta1_do(i) + dmdtheta1_leak_d(i)      ! [kg/rad]
        dedtheta1_d(i) = dmdtheta1_di(i)*h_disc - dmdtheta1_do(i)*h_dcv(i)  + dedtheta1_leak_d(i)    ! [J/rad]
        
        ! -------- for checking error -------------- !
        !write(99,2983) p_scv(i), p_dcv(i), m_dcv(i), t_scv(i), t_dcv(i)  ! for checking
        
        !if (i == 1) then
        !    dvdtheta1_d(i) = 0.0      ! [m3/rad]
        !elseif (i == no_data+1) then
        !    dvdtheta1_d(i) = ((v_com(i) - v_com(i-1))/(theta_1(i)-theta_1(i-1)))*1.d-9       ! [m3/rad]
        !else
        !    dvdtheta1_d(i) = ((v_com(i+1) - v_com(i))/(theta_1(i+1)-theta_1(i)))*1.d-9       ! [m3/rad]
        !endif
        !
        if (i == no_data+1) then
           dvdtheta1_d(i) = ((v_com(i) - v_com(i-1))/(theta_1(i)-theta_1(i-1)))*1.d-9
        else
           dvdtheta1_d(i) = ((v_com(i+1) - v_com(i))/(theta_1(i+1)-theta_1(i)))*1.d-9       ! [m3/rad]
        endif
        
        ! ------------------------------------------
        ! port opening area is changing
        ! y_mid_check is stored data only after y_start (valve tip) hit the back plate
        ! so used y_mid_check(1)
        ! ------------------------------------------
        !call discharge_valve_deflection(no_data, f_start_1, f_mid_1, f_end_1, integral_f_1, y_stop_1, dia_disc1, A_cross_dv_1, rho_dv, omega_response_dv_1(i), dpratio_dv, p_dcv_1(i), p_disc, g_r_1(i), h_r_1(i), y_start_1(i+1), y_end_1(i+1), g_r_1(i+1), h_r_1(i+1))
        call discharge_valve_deflection_ver2(area_port_open, y_mid_check(1), y_end_check(1), y_mid(i), y_start(i), F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start_dv, phi_mode_mid_dv, phi_mode_end_dv, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back, omega_natural_dv, omega_natural_dv_back, rho_dv, dpratio_dv, p_dcv(i), p_disc, g_r(i), h_r(i), y_start(i+1), y_mid(i+1), y_end(i+1), y_stop, g_r(i+1), h_r(i+1)) 
        ! ---------------------------------------------------------
        ! y_mid_check, use the point to calculate for the next point
        ! it is the value of both y_mid and y_end when y_start hit y_stop
        ! ---------------------------------------------------------
        if (y_start(i+1) >= y_stop*1.d-3) then
            y_mid_check(k) = y_mid(i+1)
            y_end_check(k) = y_end(i+1)
            k = k + 1
        endif
        !if (theta_1(i)*180.0/pi > 313.0) then
        !    y_mid(i+1) = 0.000
        !    y_start(i+1) = 0.000
        !    m_dcv(i+1) = 0.2*m_dcv(i+1)
        !endif    
        
        call next_prop_cv_d1(dqdtheta1_d(i), dmdtheta1_d(i), dedtheta1_d(i), p_dcv(i), dvdtheta1_d(i), m_dcv(i), u_dcv(i), v_com(i+1), u_dcv(i+1), m_dcv(i+1), rho_dcv(i+1), p_dcv(i+1), t_dcv(i+1), h_dcv(i+1), s_dcv(i+1), miu_dcv(i+1), cv_dcv(i+1), cp_dcv(i+1), k_dcv(i+1), v_com(i))
        
        
277 continue
        !! ---------- program error checking ---------------- !
        !do i = 1, no_data+1
        !    write(99,2983) p_dcv(i), m_dcv(i)  ! for checking
        !enddo

        print *, ' ---------------------------------------------------------------------------- '
        print '(2x,A,F10.2,A,F10.4,A)', 'Diff. between p_suc  and p_scv (Final)    = ', (p_suc - p_scv(no_data+1)), ' kPa', ((p_suc - p_scv(no_data+1))/p_suc)*100., ' % '
        !print '(2x,A,F10.2,A,F10.4,A)', 'Max. Pressure difference in suct. chamber = ', abs((p_suc - minval(p_scv))), ' kPa', abs((p_suc - minval(p_scv))/p_suc)*100., ' % '
        print '(2x,A,F10.2,A,F10.4,A)', 'Diff. between t_suc  and t_scv (Final)    = ', (t_suc - t_scv(no_data+1)), '   K', ((t_suc - t_scv(no_data+1))/t_suc)*100., ' % '
        !print '(2x,A,F10.2,A,F10.4,A)', 'Max. Temp.    difference in suct. chamber = ', abs((t_suc - minval(t_scv))), '   K', abs((t_suc - minval(t_scv))/t_suc)*100., ' % '
        print '(2x,A,F10.2,A,F10.4,A)', 'Diff. between p_disc and p_dcv (Final)    = ', (p_disc - p_dcv(no_data+1)), ' kPa', ((p_disc - p_dcv(no_data+1))/p_disc)*100., ' % '
        !print '(2x,A,F10.2,A,F10.4,A)', 'Max. Pressure difference in disc. chamber = ', abs((p_disc - maxval(p_dcv))), ' kPa', abs((p_disc - maxval(p_dcv))/p_disc)*100., ' % '
        print '(2x,A,F10.2,A,F10.4,A)', 'Diff. between t_disc and t_dcv (Final)    = ', (t_disc - t_dcv(no_data+1)), '   K', ((t_disc - t_dcv(no_data+1))/t_disc)*100., ' % '
        !print '(2x,A,F10.2,A,F10.4,A)', 'Max. Temp.    difference in disc. chamber = ', abs((t_disc - maxval(t_dcv))), '   K', abs((t_disc - maxval(t_dcv))/t_disc)*100., ' % '
        print *, ' ---------------------------------------------------------------------------- '
        
        mass_total = (m_dcv(1)-mass_residual_dead) * freq   ! [kg/s]
        mass_suct_in = m_scv(no_data+1)*freq ! [kg/s]
        ! --------------------------------------
        ! Record down residual mass
        ! Residual mass re-expand to suction volume
        ! May cause return flow in suction flow through suction port
        ! --------------------------------------
        mass_residual_dead = m_dcv(no_data) 
        h_residual_dead = h_dcv(no_data)
        
        
2981 format (14A25)
2982 format (F25.4, 14ES25.6)
2983 format (14ES25.6)     
end subroutine thermo_with_heat_leakage
