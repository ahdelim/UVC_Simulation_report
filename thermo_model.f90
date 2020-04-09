subroutine thermo_m(theta_1, theta_2, dlvdt, dgammadt_1, v_com, v_suc, l_v, p_cscv_id, p_cdcv_id, dvdtheta1_s, dvdtheta1_d, dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
    implicit none
    integer, parameter :: k_stop = 15
    integer i
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"
    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    include "var_geometrical_diff.f90"
    include "var_compressor_evaluation.f90"
    include "var_thermo.f90"
    include "var_thermo_error_criterion.f90"
    include "var_thermo_error_ptr.f90"
    include "var_indicated_work.f90"
    include "var_reed_valve.f90"
    double precision, dimension (1:no_data+1) :: dlvdt, dgammadt_1, dgammadtt_1
    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id
    double precision, dimension (1:no_data+1) :: y_start, y_mid, y_end
    ! --- Variables for Leakage --- !
    double precision, dimension (1:no_data+1) :: dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d
    double precision, dimension (1:no_data+1) :: dmdtheta1_leak_rad_ro, dmdtheta1_leak_vef, dmdtheta1_leak_vs, dmdtheta1_leak_ref
    ! --- Total Heat Transfer in various chambers
    double precision, dimension (1:no_data+1) :: dqdtheta_sc, dqdtheta_dc, dqdtheta_hc, dqdtheta_resoil, dqdtheta_oil
    double precision, dimension (1:no_data+1) :: T_ocsc, T_ocdc, T_rosc, T_rodc, T_roller, T_uocsc, T_locsc, T_uocdc, T_locdc
    double precision, dimension (1:no_data+1) :: T_ocsc2, T_ocsc3, T_ocdc2, T_ocdc3, T_rosc2, T_rosc3, T_rodc2, T_rodc3, T_roller2, T_roller3
    double precision, dimension (1:no_data+1) :: Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex   ! variables used by exergy
    integer k, journal_opt, heat_opt, leakage_opt
    common/model_opt/journal_opt, heat_opt, leakage_opt
    double precision, dimension (1:no_data+1) :: F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, T_inertia_ro, T_inertia_vh, T_com_C
    double precision, dimension (1:no_data+1) :: eccr, att, h_min, p_max, Q_martin, cl_rotor_rad_dy
    
    ! ------------------------ Runge Kutta Coefficients ------------------------ !
    call Runge_steps_coefficients       ! runge-kutta step and coefficients
    
    ! ------------------------ Valve parameters & mode shape ------------------- !
    call discharge_valve_parameters(no_data, A_cross_dv_1, I_dv_1, beta_r_1)
    call discharge_valve_parameters_ver2(w_dv, t_dv, l_dv_ef, w_dv_x2, r_dv_x1, l_dv_x2, l_dv_x3, omega_natural_dv, omega_natural_dv_back)
    !call discharge_valve_mode_shape(beta_r_1, x_hole_start_1, l_dv_1, dia_disc1, f_start_1, f_mid_1, f_end_1, integral_f_1)
    call discharge_valve_mode_shape_ver2(t_dv, w_dv_x2, w_dv, l_dv_ef, r_dv_x1, l_dv_x2, l_dv_x3, dia_disc, F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start_dv, phi_mode_mid_dv, phi_mode_end_dv, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back)
    
    ! ------------------------ Thermodynamics Proerties (adiabatic and no internal leakage) ------------------------ !
    print *, ' >>>>> Thermodynamic: Adiabatic and no internal leakage '
    call thermo_no_heat_leakage(theta_1, v_com, v_suc, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dmdtheta1_s, dedtheta1_s, dmdtheta1_d, dedtheta1_d, dvdtheta1_s, dvdtheta1_d, y_start, y_mid, y_end) 
        
    print *, ' End of Thermodynamic: Adiabatic and no internal leakage '
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' '
    print *, ' '
   
    
    ! ------------------------ Indicated work for the adiabatic and no internal leakage -------------------------- !
    call indicated_work(v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, dvdtheta1_s, dvdtheta1_d, P_ind, P_ind_id, P_loss_suc, P_loss_disc, P_comp_loss)
    
    ! ----------------------------------------------------------------------------!
    ! Data writing 
    ! Thermodynamics properties for adiabatic and no leakage case
    ! ----------------------------------------------------------------------------!
        write (73,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
        write (74,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
        write (71,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
        write (72,2981), "Volume[m3]", "Pressure[kPa]"
        write (80,2981), "Degree", "dmdtheta1", "dedtheta1", "dvdtheta1"
        !write (70,2981), "Degree", "dmdtheta1_s", "dedtheta1_s", "dvdtheta1_s"
        !write (79,2981), "Degree", "dmdtheta1_d", "dedtheta1_d", "dvdtheta1_d"
        !write (82,2981), "Degree", "y_start[mm]", "y_stop[mm]"
        write (82,2981), "Degree", "y_mid[mm]", "y_start[mm]"
    

        ! ----------------------------------------------------------------------------!
        ! NVC Compressor Suction (w/o heat transfer and leakage)
        ! ----------------------------------------------------------------------------!
        !do 291 i = 1, max_write_data, data_step_suct
        do 291 i = 1, no_data+data_step_suct, data_step_suct
            write(80,2982) theta_1(i)*180./pi, dmdtheta1_s(i),dedtheta1_s(i), dvdtheta1_s(i)
            write(71,2982) theta_1(i)*180./pi, v_suc(i)*1.d-9,p_scv(i), t_scv(i), u_scv(i), rho_scv(i),h_scv(i), s_scv(i), miu_scv(i), cp_scv(i), k_scv(i), m_scv(i), y_mid(1)*1000., y_start(1)*1000.
            write(72,2983) v_suc(i)*1.d-9, p_scv(i)
            write(73,2982) theta_1(i)*180./pi, v_suc(i)*1.d-9,p_scv(i), t_scv(i), u_scv(i), rho_scv(i), h_scv(i), s_scv(i), miu_scv(i), cp_scv(i), k_scv(i), m_scv(i), y_mid(1)*1000., y_start(1)*1000.
            !write(82,2982) theta_1(i)*180./pi, y_start_1(i)*1000., y_end_1(i)*1000.
            write(82,2982) theta_1(i)*180./pi, y_mid(1)*1000., y_start(1)*1000.
291 continue   
    
        ! ----------------------------------------------------------------------------!
        ! NVC Compressor compression (discharge) (w/o heat transfer and leakage)
        ! ----------------------------------------------------------------------------!
        !do 292 i = 1, max_write_data, data_step
        do 292 i = 1, no_data+data_step, data_step
            !write(70,2982) theta_1(i)*180./pi, dmdtheta1_s_1(i),dedtheta1_s_1(i), dvdtheta1_s_1(i)
            write(80,2982) 360.+theta_1(i)*180./pi, dmdtheta1_d(i), dedtheta1_d(i),dvdtheta1_d(i)
            write(71,2982) 360.+theta_1(i)*180./pi, v_com(i)*1.d-9, p_dcv(i), t_dcv(i), u_dcv(i), rho_dcv(i),h_dcv(i), s_dcv(i), miu_dcv(i), cp_dcv(i), k_dcv(i), m_dcv(i), y_mid(i)*1000., y_start(i)*1000.
            write(74,2982) theta_1(i)*180./pi, v_com(i)*1.d-9, p_dcv(i), t_dcv(i), u_dcv(i), rho_dcv(i), h_dcv(i), s_dcv(i), miu_dcv(i), cp_dcv(i), k_dcv(i), m_dcv(i), y_mid(i)*1000., y_start(i)*1000.
            write(72,2983) v_com(i)*1.d-9, p_dcv(i)
            !write(82,2982) 360+theta_1(i)*180/pi, y_start_1(i)*1000., y_end_1(i)*1000.
            write(82,2982) 360.+theta_1(i)*180./pi, y_mid(i)*1000., y_start(i)*1000.
292 continue 
    ! ===========================================================================
    ! ---------------------------------------------------------------------------
    
    ! Thermodynamic with heat transfer and leakage
    ! re-expansion model included
    
    ! --------------------------------------------------------------------------
    ! ------------------------ Declare Error Parameter ------------------------- !
    call thermo_error_para(k)
    ! -----------------------------------------
    ! Initialise residual mass and enthalpy
    ! residual mass is caused by dead volume
    ! -----------------------------------------
    print *, ' '
    print *, ' >>>>> Thermodynamic (Actual): With Heat Transfer and Leakage '
    print *, ' '
    mass_residual_dead = 0.0
    h_residual_dead = 0.0
    
    
    ! ------------------------------
    ! initialise leakage and heat transfer variables
    ! ------------------------------
    do i = 1, no_data+1
        ! ----- Leakage variables ------ !
        dmdtheta1_leak_s(i) = 0.0
        dedtheta1_leak_s(i) = 0.0
        dmdtheta1_leak_d(i) = 0.0
        dedtheta1_leak_d(i) = 0.0
        dmdtheta1_leak_rad_ro(i) = 0.0
        dmdtheta1_leak_vef(i) = 0.0
        dmdtheta1_leak_vs(i) = 0.0
        
        ! ----- Heat Transfer variables ----- !
        dqdtheta_sc(i) = 0.0
        dqdtheta_dc(i) = 0.0
        dqdtheta_hc(i) = 0.0
        dqdtheta_resoil(i) = 0.0
        dqdtheta_oil(i) = 0.0
        
        cl_rotor_rad_dy = 10*1d-6
    ! ----------- calculate first iteration leakage and heat transfer
    enddo
    if (leakage_opt == 1) then
        print *, ' Running Leakage Model (1st Trial)... '
        call leakage_m(theta_1, theta_2, p_scv, t_scv, rho_scv, p_dcv, t_dcv, cv_dcv, cp_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, l_v, cl_rotor_rad_dy, dmdtheta1_leak_rad_ro, dmdtheta1_leak_vef, dmdtheta1_leak_vs, dmdtheta1_leak_ref, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d)
    endif
    if (heat_opt == 1) then
        print *, ' Running Heat Transfer Model (1st Trial)... '
        call heat_transfer_m(theta_1, theta_2, v_com, v_suc, dlvdt, dgammadt_1, l_v, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dqdtheta_sc, dqdtheta_dc, dqdtheta_hc, dqdtheta_resoil, dqdtheta_oil, T_ocsc, T_ocdc, T_rosc, T_rodc, T_roller, T_uocsc, T_locsc, T_uocdc, T_locdc, T_ocsc2, T_ocsc3, T_ocdc2, T_ocdc3, T_rosc2, T_rosc3, T_rodc2, T_rodc3, T_roller2, T_roller3, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
    endif
    do while (abs(error_pos) > conv_criterion .or. abs(error_poc) > conv_criterion .or. abs(error_tos) > conv_criterion .or. abs(error_toc) > conv_criterion)
        
        print *, ' ---------------------------------------------------------------------------- '
        print '(2x,A,I3)', ' Iteration ',k
        
        print *, ' '
        ! ------------------------ Thermodynamics Properties (Re-expansion/residual mass + internal leakage + heat transfer) ------------------------------ !
        call thermo_with_heat_leakage(theta_1, v_com, v_suc, dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_di, dmdtheta1_do, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dmdtheta1_s, dedtheta1_s, dmdtheta1_d, dedtheta1_d, dvdtheta1_s, dvdtheta1_d, y_start, y_mid, y_end)
        call dynamic_m2(theta_1, theta_2, gamma_1, l_v, p_scv, p_dcv, dgammadtt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, T_inertia_ro, T_inertia_vh, T_com_C)
        call journal_bear_hirani_2(theta_1, dgammadt_1, F_cx, F_cy, F_resultant, eccr, att, h_min, p_max, Q_martin, cl_rotor_rad_dy)
        
        ! ------------------------ Leakage Model ----------------------------------- !
        if (leakage_opt == 1) then
            print *, ' Running Leakage Model ... '
            !call leakage_m(theta_1, theta_2, p_scv, t_scv, rho_scv, p_dcv, t_dcv, cv_dcv, cp_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, l_v, dmdtheta1_leak_rad_ro, dmdtheta1_leak_vef, dmdtheta1_leak_vs, dmdtheta1_leak_ref, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d)
            call leakage_m(theta_1, theta_2, p_scv, t_scv, rho_scv, p_dcv, t_dcv, cv_dcv, cp_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, l_v, cl_rotor_rad_dy, dmdtheta1_leak_rad_ro, dmdtheta1_leak_vef, dmdtheta1_leak_vs, dmdtheta1_leak_ref, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d)
        endif
        
        ! ------------------------ Heat Transfer Model ----------------------------------- !
        if (heat_opt == 1) then
            print *, ' Running Heat Transfer Model ... '
            call heat_transfer_m(theta_1, theta_2, v_com, v_suc, dlvdt, dgammadt_1, l_v, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dqdtheta_sc, dqdtheta_dc, dqdtheta_hc, dqdtheta_resoil, dqdtheta_oil, T_ocsc, T_ocdc, T_rosc, T_rodc, T_roller, T_uocsc, T_locsc, T_uocdc, T_locdc, T_ocsc2, T_ocsc3, T_ocdc2, T_ocdc3, T_rosc2, T_rosc3, T_rodc2, T_rodc3, T_roller2, T_roller3, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
        endif
        ! ------------------------ Error checking ---------------------------------- !
        call thermo_error_check(k, k_stop, p_scv, p_dcv, t_scv, t_dcv, p_test_suct, p_test_comp, t_test_suct, t_test_comp)
        
        ! ---------- program error checking ---------------- !
        !do i = 1, no_data+1
        !    write(99,2992) dmdtheta1_leak_s(i), dmdtheta1_leak_d(i)  ! for checking
        !enddo
        !print*, (sum(dmdtheta1_leak_s)/(no_data+1))*omega_1
        k = k + 1
        if (k > k_stop) then
            print *, ' End of Thermodynamic (Actual): With heat transfer and leakage '
            print *, ' ---------------------------------------------------------------------------- '
            print *, ' '
            exit
        endif
        print *, ' '
        print *, ' '
        print *, ' '
    end do
    
    
    
    
    
    
    ! ----------------------------------------------------------------------------!
    ! Data writing !
    ! UVC Thermodynamics properties (with heat transfer and internal leakage)
    ! ----------------------------------------------------------------------------!
    
    write (77,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
    write (76,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
    write (75,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
    write (78,2981), "Volume[m3]", "Pressure[kPa]"
    write (81,2981), "Degree", "dmdtheta1", "dedtheta1", "dvdtheta1"
    !write (70,2981), "Degree", "dmdtheta1_s", "dedtheta1_s", "dvdtheta1_s"
    !write (79,2981), "Degree", "dmdtheta1_d", "dedtheta1_d", "dvdtheta1_d"
    !write (82,2981), "Degree", "y_start[mm]", "y_stop[mm]"
    write (83,2981), "Degree", "y_mid[mm]", "y_start[mm]"
    write (95,2981), "Degree", "Press.Ratio", "Mass_leak_rad_ro[kg/s]", "Mass_leak_vef[kg/s]", "Mass_leak_vs[kg/s]", "Mass_leak_ref[kg/s]", "Mass_leak_total[kg/s]"
    write (96,2981), "Degree", "dqdtheta_sc[W]", "dqdtheta_dc[W]", "dqdtheta_hc[W]", "dqdtheta_resoil[W]", "dqdtheta_oil[W]"
    write (97,2981), "Degree", "T_ocsc[K]", "T_ocdc[K]", "T_rosc[K]", "T_rodc[K]", "T_roller[K]", "T_uocsc[K]", "T_locsc[K]", "T_uocdc[K]", "T_locdc[K]"
    write (98,2981), "Degree", "T_ocsc2[K]", "T_ocsc3[K]", "T_ocdc2[K]", "T_ocdc3[K]", "T_rosc2[K]", "T_rosc3[K]", "T_rodc2[K]", "T_rodc3[K]", "T_roller2[K]", "T_roller3[K]"

    ! ----------------------------------------------------------------------------!
    ! UVC Compressor Suction
    ! ----------------------------------------------------------------------------!
    !do 291 i = 1, max_write_data, data_step_suct
    do 293 i = 1, no_data+data_step_suct, data_step_suct
        write(81,2982) theta_1(i)*180./pi, dmdtheta1_s(i),dedtheta1_s(i), dvdtheta1_s(i)
        write(75,2982) theta_1(i)*180./pi, v_suc(i)*1.d-9,p_scv(i), t_scv(i), u_scv(i), rho_scv(i),h_scv(i), s_scv(i), miu_scv(i), cp_scv(i), k_scv(i), m_scv(i), y_mid(1)*1000., y_start(1)*1000.
        write(78,2983) v_suc(i)*1.d-9, p_scv(i)
        write(77,2982) theta_1(i)*180./pi, v_suc(i)*1.d-9,p_scv(i), t_scv(i), u_scv(i), rho_scv(i), h_scv(i), s_scv(i), miu_scv(i), cp_scv(i), k_scv(i), m_scv(i), y_mid(1)*1000., y_start(1)*1000.
        !write(82,2982) theta_1(i)*180./pi, y_start_1(i)*1000., y_end_1(i)*1000.
        write(83,2982) theta_1(i)*180./pi, y_mid(1)*1000., y_start(1)*1000.
        
293 continue   
    ! ----------------------------------------------------------------------------!
    ! UVC Compressor compression (discharge)
    ! ----------------------------------------------------------------------------!
    !do 292 i = 1, max_write_data, data_step
    do 294 i = 1, no_data+data_step, data_step
        !write(70,2982) theta_1(i)*180./pi, dmdtheta1_s_1(i),dedtheta1_s_1(i), dvdtheta1_s_1(i)
        write(81,2982) 360.+theta_1(i)*180./pi, dmdtheta1_d(i), dedtheta1_d(i),dvdtheta1_d(i)
        write(75,2982) 360.+theta_1(i)*180./pi, v_com(i)*1.d-9, p_dcv(i), t_dcv(i), u_dcv(i), rho_dcv(i),h_dcv(i), s_dcv(i), miu_dcv(i), cp_dcv(i), k_dcv(i), m_dcv(i), y_mid(i)*1000., y_start(i)*1000.
        write(76,2982) theta_1(i)*180./pi, v_com(i)*1.d-9, p_dcv(i), t_dcv(i), u_dcv(i), rho_dcv(i), h_dcv(i), s_dcv(i), miu_dcv(i), cp_dcv(i), k_dcv(i), m_dcv(i), y_mid(i)*1000., y_start(i)*1000.
        write(78,2983) v_com(i)*1.d-9, p_dcv(i)
        !write(82,2982) 360+theta_1(i)*180/pi, y_start_1(i)*1000., y_end_1(i)*1000.
        write(83,2982) 360.+theta_1(i)*180./pi, y_mid(i)*1000., y_start(i)*1000.
        ! write data into test files  below
        
294 continue 
    ! ------------------------------------------------------------------ !
    ! Leakage and Heat Transfer data writing
    ! ------------------------------------------------------------------ !
    do 295 i = 1, no_data+data_step, data_step
    !do 295 i = 1, no_data, data_step
        write(95,2982) theta_1(i)*180./pi, p_dcv(i)/p_scv(i), dmdtheta1_leak_rad_ro(i)*omega_1, dmdtheta1_leak_vef(i)*omega_1, dmdtheta1_leak_vs(i)*omega_1, dmdtheta1_leak_ref(i)*omega_1, dmdtheta1_leak_s(i)*omega_1
        write(96,2982) theta_1(i)*180./pi, dqdtheta_sc(i)*omega_1, dqdtheta_dc(i)*omega_1, dqdtheta_hc(i)*omega_1, dqdtheta_resoil(i)*omega_1, dqdtheta_oil(i)*omega_1
        write(97,2982) theta_1(i)*180./pi, T_ocsc(i), T_ocdc(i), T_rosc(i), T_rodc(i), T_roller(i), T_uocsc(i), T_locsc(i), T_uocdc(i), T_locdc(i)
        write(98,2982) theta_1(i)*180./pi, T_ocsc2(i), T_ocsc3(i), T_ocdc2(i), T_ocdc3(i), T_rosc2(i), T_rosc3(i), T_rodc2(i), T_rodc3(i), T_roller2(i), T_roller3(i)
        write(162,*) cl_rotor_rad_dy(i)
295 continue 
    ! non-deal mass flow rate
    
    !write(42,2981) 'Mass_Flow_1[kg/s]', 'Mass_Flow_2[kg/s]', 'Mass_Flow_Total[kg/s]', ' Inlet Spec. Enthalpy[J/kg]'
    !write(42,2983) mass_outer, mass_inner, mass_total, h_bef_evap
    
    
2981 format (16(A25','))
2982 format (F25.4',', 15(ES25.6','))    
2983 format (14(ES25.6',')) 
2989 format (2x,A,F8.4,A)
2992 format (14(ES25.6',')) 
endsubroutine thermo_m
