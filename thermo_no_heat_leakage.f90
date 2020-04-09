subroutine thermo_no_heat_leakage(theta_1, v_com, v_suc, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dmdtheta1_s, dedtheta1_s, dmdtheta1_d, dedtheta1_d, dvdtheta1_s, dvdtheta1_d, y_start, y_mid, y_end)
    
    ! --------------------------------------
    !   Note:
    !   In this thermo model, discharge port extends to the end of cycle
    !   No left over mass
    ! --------------------------------------
    
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
    
    ! Thermodynamic Variables
    include "var_thermo.f90"
    
    ! Compressor Performance
    include "var_compressor_evaluation.f90"
    
    ! Discharge valve Variables
    !double precision, dimension (1:no_data+1) :: omega_response_dv_1                                                                ! for rectangular valve
    !double precision f_start_1, f_mid_1, f_end_1, omega_natural_dv_1, A_cross_dv_1, I_dv_1, beta_r_1, integral_f_1                  ! for rectangular valve
    double precision, dimension (1:no_data+1) :: g_r, h_r, y_start, y_mid, y_end                                           ! for ver2, g = q, h = dq/dt
    double precision, dimension (1:no_data+1) :: y_mid_check, y_end_check

    double precision area_port_open ! discharge port opening area
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
    cv_scv(1) = cv_suc  ![J/kg.K]
    cp_scv(1) = cp_suc ![J/kg.K]
    k_scv(1) = k_suc !W/(m.K)
    m_scv(1) = rho_scv(1) * v_suc(1)*1.d-9  ![kg]

    ! --------------------------------------------------------
    ! Thermodynamic Equation
    ! --------------------------------------------------------
    do 271 i = 1, no_data+1
        if (p_suc > p_scv(i)) then
            !call m_flow_suc(omega_1, coef_sucport, p_scv(i), rho_suc, s_suc, h_suc, dia_suc, dmdtheta1_si(i))         ! Mass Flow Model for suction
            call m_flow_suc_ver2(r_oc, theta_1(i), omega_1, coef_sucport, p_scv(i), rho_suc, s_suc, h_suc, dia_suc, dmdtheta1_si(i), theta_suc_start, theta_suc_end)
            dmdtheta1_so(i) = 0
        else
            dmdtheta1_si(i) = 0
            !call m_flow_suc(omega_1, coef_sucport, p_suc, rho_scv(i), s_scv(i), h_scv(i), dia_suc, dmdtheta1_so(i))
            call m_flow_suc_ver2(r_oc, theta_1(i), omega_1, coef_sucport, p_suc, rho_scv(i), s_scv(i), h_scv(i), dia_suc, dmdtheta1_so(i), theta_suc_start, theta_suc_end)
        endif
        
        dqdtheta1_s(i) = 0.0              ! Assume adiabatic
        dmdtheta1_s(i) = dmdtheta1_si(i) - dmdtheta1_so(i)  ! + dmdtheta1_leak_s(i)      [kg/rad]
        dedtheta1_s(i) = dmdtheta1_si(i)*h_suc - dmdtheta1_so(i)*h_scv(i) ! + dedtheta1_leak_s(i)    [J/rad]
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
        
        !y_start_1(i) = 0.0          ! suction (valve displacement = 0)
        !y_mid_1(i) = 0.0
        !y_end_1(i) = 0.0            ! suction (valve displacement = 0)
          
        
        ! ----------- Write every data
        !write(70,2982) theta_1(i)*180/pi, dmdtheta1_s_1(i),dedtheta1_s_1(i), dvdtheta1_s_1(i)
        !write(71,2982) theta_1(i)*180/pi, v_suc1(i)*1.d-9,p_scv_1(i), t_scv_1(i), u_scv_1(i), rho_scv_1(i),h_scv_1(i), s_scv_1(i), miu_scv_1(i), cp_scv_1(i), k_scv_1(i), m_scv_1(i)
        !write(72,2983) v_suc1(i)*1.d-9, p_scv_1(i)
        !write(73,2982) theta_1(i)*180/pi, v_suc1(i)*1.d-9,p_scv_1(i), t_scv_1(i), u_scv_1(i), rho_scv_1(i),h_scv_1(i), s_scv_1(i), miu_scv_1(i), cp_scv_1(i), k_scv_1(i), m_scv_1(i)
        !write(82,2982) theta_1(i)*180/pi, y_start_1(i), y_end_1(i)
        
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
    cv_dcv(1) = cv_scv(no_data+1)       ![J/kg.K]
    cp_dcv(1) = cp_scv(no_data+1)       ![J/kg.K]
    k_dcv(1) = k_scv(no_data+1)         !W/(m.K)
    m_dcv(1) = m_scv(no_data+1)         ![kg]
    
    g_r(1) = 0.0        ! initial modal displacement
    h_r(1) = 0.0        ! initial modal velocity (h = dg/dt)
    y_start(1) = 0.0
    y_mid(1) = 0.0
    y_end(1) = 0.0

    k = 1
    do 277 i = 1, no_data+1
    if (theta_1(i) <= theta_suc_end) then
        p_dcv(i) = p_scv(no_data+1)
    endif
        if (p_dcv(i) > p_disc) then
            dmdtheta1_di(i) = 0.0
            call m_flow_disc(omega_1, coef_discport, y_mid(i), p_disc, rho_dcv(i), s_dcv(i), h_dcv(i), dia_disc, dmdtheta1_do(i), area_port_open)
            !call m_flow_disc_ver2(r_oc, theta_1(i), omega_1, coef_discport, y_mid(i), p_disc, rho_dcv(i), s_dcv(i), h_dcv(i), dia_disc, dmdtheta1_do(i), theta_disc_start, theta_disc_end, area_port_open)
        else
            call m_flow_disc(omega_1, coef_discport, y_mid(i), p_dcv(i), rho_disc, s_disc, h_disc, dia_disc, dmdtheta1_di(i), area_port_open)
            !call m_flow_disc_ver2(r_oc, theta_1(i), omega_1, coef_discport, y_mid(i), p_dcv(i), rho_disc, s_disc, h_disc, dia_disc, dmdtheta1_di(i), theta_disc_start, theta_disc_end, area_port_open)
            dmdtheta1_do(i) = 0.0
        endif
        
        dqdtheta1_d(i) = 0.0            ! Heat transfer
        dmdtheta1_d(i) = dmdtheta1_di(i) - dmdtheta1_do(i)  ! + dmdtheta1_leak_d_1(i)      [kg/rad]
        dedtheta1_d(i) = dmdtheta1_di(i)*h_disc - dmdtheta1_do(i)*h_dcv(i) ! + dedtheta1_leak_d_1(i)    [J/rad]
        
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
        
        !call discharge_valve_deflection(no_data, f_start_1, f_mid_1, f_end_1, integral_f_1, y_stop_1, dia_disc1, A_cross_dv_1, rho_dv, omega_response_dv_1(i), dpratio_dv, p_dcv_1(i), p_disc, g_r_1(i), h_r_1(i), y_start_1(i+1), y_end_1(i+1), g_r_1(i+1), h_r_1(i+1))
        ! ------------------------------------------------------------
        ! Assuming area port open is always fully open in this routine
        ! ------------------------------------------------------------
        call discharge_valve_deflection_ver2(area_port_open, y_mid_check(1), y_end_check(1), y_mid(i), y_start(i), F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start_dv, phi_mode_mid_dv, phi_mode_end_dv, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back, omega_natural_dv, omega_natural_dv_back, rho_dv, dpratio_dv, p_dcv(i), p_disc, g_r(i), h_r(i), y_start(i+1), y_mid(i+1), y_end(i+1), y_stop, g_r(i+1), h_r(i+1)) 
        ! ---------------------------------------------------------
        ! y_mid_check, use the point to calculate for the next point
        ! it is the value of y_mid and y_end when y_start hit y_stop
        ! ---------------------------------------------------------
        if (y_start(i+1) >= y_stop*1.d-3) then
            y_mid_check(k) = y_mid(i+1)
            y_end_check(k) = y_end(i+1)
            k = k + 1
        endif
        
        call next_prop_cv_d1(dqdtheta1_d(i), dmdtheta1_d(i), dedtheta1_d(i), p_dcv(i), dvdtheta1_d(i), m_dcv(i), u_dcv(i), v_com(i+1), u_dcv(i+1), m_dcv(i+1), rho_dcv(i+1), p_dcv(i+1), t_dcv(i+1), h_dcv(i+1), s_dcv(i+1), miu_dcv(i+1), cv_dcv(i+1), cp_dcv(i+1), k_dcv(i+1), v_com(i))

    !else
        ! ---------------------------------------------
        ! check point when the discharge port is fully closed
        ! 
        
277 continue
    ! --------------------------------------------------------
    !    write file
    ! --------------------------------------------------------
!    do 292 i = 1, max_write_data
!        write(79,2982) theta_1(data_step*i)*180/pi, dmdtheta1_d_1(data_step*i), dedtheta1_d_1(data_step*i),dvdtheta1_d_1(data_step*i)
!        write(71,2982) 360+theta_1(data_step*i)*180/pi, v_com1(data_step*i)*1.d-9, p_dcv_1(data_step*i), t_dcv_1(data_step*i), u_dcv_1(data_step*i), rho_dcv_1(data_step*i),h_dcv_1(data_step*i), s_dcv_1(data_step*i), miu_dcv_1(data_step*i), cp_dcv_1(data_step*i), k_dcv_1(data_step*i), m_dcv_1(data_step*i)
!        write(74,2982), theta_1(data_step*i)*180/pi, v_com1(data_step*i)*1.d-9,p_dcv_1(data_step*i), t_dcv_1(data_step*i), u_dcv_1(data_step*i), rho_dcv_1(data_step*i),h_dcv_1(data_step*i), s_dcv_1(data_step*i), miu_dcv_1(data_step*i), cp_dcv_1(data_step*i), k_dcv_1(data_step*i), m_dcv_1(data_step*i)
!        write(72,2983) v_com1(data_step*i)*1.d-9, p_dcv_1(data_step*i)
!        write(82,2982) 360+theta_1(data_step*i)*180/pi, y_start_1(data_step*i), y_end_1(data_step*i)
!292 continue 
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
        
        mass_total = m_dcv(1) * freq
        
2981 format (14A25)
2982 format (F25.4, 14ES25.6)
2983 format (14ES25.6)     
end subroutine thermo_no_heat_leakage

    
    
    
    
    
    
    ! write file 
!    do 291 i = 1, max_write_data
!        write(70,2982) theta_1(data_step*i)*180/pi, dmdtheta1_s_1(data_step*i),dedtheta1_s_1(data_step*i), dvdtheta1_s_1(data_step*i)
!        write(71,2982) theta_1(data_step*i)*180/pi, v_suc1(data_step*i)*1.d-9,p_scv_1(data_step*i), t_scv_1(data_step*i), u_scv_1(data_step*i), rho_scv_1(data_step*i),h_scv_1(data_step*i), s_scv_1(data_step*i), miu_scv_1(data_step*i), cp_scv_1(data_step*i), k_scv_1(data_step*i), m_scv_1(data_step*i)
!        write(72,2983) v_suc1(data_step*i)*1.d-9, p_scv_1(data_step*i)
!        write(73,2982) theta_1(data_step*i)*180/pi, v_suc1(data_step*i)*1.d-9,p_scv_1(data_step*i), t_scv_1(data_step*i), u_scv_1(data_step*i), rho_scv_1(data_step*i),h_scv_1(data_step*i), s_scv_1(data_step*i), miu_scv_1(data_step*i), cp_scv_1(data_step*i), k_scv_1(data_step*i), m_scv_1(data_step*i)
!        write(82,2982) theta_1(data_step*i)*180/pi, y_start_1(data_step*i), y_end_1(data_step*i)
!291 continue   
            
        ! to write discharge fluid properties
        !write(79,2982) theta_1(i)*180/pi, dmdtheta1_d_1(i), dedtheta1_d_1(i),dvdtheta1_d_1(i)
        !write(71,2982) 360+theta_1(i)*180/pi, v_com1(i)*1.d-9, p_dcv_1(i), t_dcv_1(i), u_dcv_1(i), rho_dcv_1(i),h_dcv_1(i), s_dcv_1(i), miu_dcv_1(i), cp_dcv_1(i), k_dcv_1(i), m_dcv_1(i)
        !write(74,2982), theta_1(i)*180/pi, v_com1(i)*1.d-9,p_dcv_1(i), t_dcv_1(i), u_dcv_1(i), rho_dcv_1(i),h_dcv_1(i), s_dcv_1(i), miu_dcv_1(i), cp_dcv_1(i), k_dcv_1(i), m_dcv_1(i)
        !write(72,2983) v_com1(i)*1.d-9, p_dcv_1(i)
        !write(82,2982) 360+theta_1(i)*180/pi, y_start_1(i), y_end_1(i)