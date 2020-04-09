!    ! --------------------------------------------------------
!! This file contains 5 subroutines
!! 1.    main thermo
!! 2.    Geometrical
!! 3.    Kinematics
!! 4.    thermo_id
!! 5.    thermo_heat_leakage
!! 6.    indicated_work
!! 7.    power model
!! --------------------------------------------------------
!subroutine GA_thermo_main()
!    implicit none
!    include "var_simulation_parameter.f90"
!    include "var_operational_parameter.f90"
!    ! --- Geometrical ----
!    double precision, dimension (1:no_data+1) :: theta_1, theta_2, l_v, v_com, v_suc, gamma_1
!    ! --- Kinematics ---
!    double precision, dimension (1:no_data+1) :: dvdtheta1_suc, dvdtheta1_com      ! differentiated volume
!    double precision, dimension (1:no_data+1) :: dvdtheta1_s, dvdtheta1_d          ! differentiated volume
!    double precision, dimension (1:no_data+1) :: dgammadtheta1_1
!    double precision, dimension (1:no_data+1) :: dgammadt_1, dgammadtt_1        ! gamma_1 speed and acceleration
!    double precision, dimension (1:no_data+1) :: dlvdtheta1, dlvdtheta12        ! vane differentiated w.r.t theta_1
!    double precision, dimension (1:no_data+1) :: dlvdt, dlvdtt      ! vane speed and acceleration
!    ! --- Ideal Thermodynamic Variables
!    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id        ! Ideal compressor suction/discharge pressure
!    ! --- Thermodynamic Variables
!    double precision, dimension (1:no_data+1) :: p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv
!    double precision, dimension (1:no_data+1) :: p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv
!    double precision, dimension (1:no_data+1) :: Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex       ! for exergy calculation
!    double precision, dimension (1:no_data+1) :: dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do
!    ! --- Dynamic Variables
!    double precision, dimension (1:no_data+1) :: F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant       ! Forces to be determined (for 6 unknowns)
!    double precision, dimension (1:no_data+1) :: T_inertia_ro, T_inertia_vh, T_com_C, T_total_no_loss
!    double precision, dimension (1:no_data+1) :: I_ro_O
!    ! --- Frictional loss
!    double precision, dimension (1:no_data+1) :: L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub       ! Losses definition
!    ! --- Journal Bearing Variables
!    double precision, dimension (1:no_data+1) :: eccr, att, h_min, p_max, Q_martin
!    ! --- Performance variables
!    double precision, dimension (1:no_data+1) :: P_bear_s
!    
!    
!    
!    ! --------------------------------------------------------
!    ! Mathematical model starts here
!    ! --------------------------------------------------------
!    ! ----- Geometrical
!    call optimization_geo_m(theta_1, theta_2, l_v, v_com, v_suc, gamma_1)
!    ! ----- Kinematics
!    call optimization_kinematics_m(theta_1, gamma_1, l_v, dlvdt, dlvdtt, dgammadt_1, dgammadtt_1, dvdtheta1_suc, dvdtheta1_com)
!    ! ----- Thermodynamic Ideal and Non-ideal
!    call optimization_thermo_id(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id)
!    call optimization_thermo_m(theta_1, theta_2, dlvdt, dgammadt_1, v_com, v_suc, l_v, p_cscv_id, p_cdcv_id, dvdtheta1_s, dvdtheta1_d, dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
!    ! ----- Indicated work for the adiabatic and no internal leakage -------------------------- !
!    !call optimization_indicated_work(v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, dvdtheta1_s, dvdtheta1_d, P_ind, P_ind_id, P_loss_suc, P_loss_disc, P_comp_loss)
!    ! ----- Dynamic (original)
!    call dynamic_m(theta_1, theta_2, gamma_1, l_v, p_scv, p_dcv, dgammadtt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, T_inertia_ro, T_inertia_vh, T_com_C)
!    ! ----- Journal Bearing (original)
!    call journal_bear_hirani(theta_1, F_cx, F_cy, F_resultant, eccr, att, h_min, p_max, Q_martin)
!    ! ----- Power loss
!    call optimization_power_m(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, h_scv, h_dcv, dmdtheta1_si, dmdtheta1_so, dmdtheta1_di, dmdtheta1_do, dvdtheta1_s, dvdtheta1_d, dlvdt, dgammadt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, eccr, att, T_inertia_ro, T_inertia_vh, T_com_C, L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s)
!    ! ----- Exergy 
!    call optimization_exergy(theta_1, v_com, v_suc, p_scv, p_dcv, t_scv, t_dcv, s_scv, s_dcv, h_scv, h_dcv, u_scv, u_dcv, m_scv, m_dcv, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, dqdtheta_sc, dqdtheta_dc, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex, L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s)
!    
!endsubroutine GA_thermo_main
!    
!    
!    
!subroutine optimization_geo_m(theta_1, theta_2, l_v, v_com, v_suc, gamma_1)
!    
!    include "var_simulation_parameter.f90"
!    include "var_main_dimensions.f90"
!    include "var_geometrical.f90"
!    include "var_operational_parameter.f90"
!    include "var_physical_constants.f90"
!    include "var_compressor_evaluation.f90"
!    
!    ! Define theta_1
!    call geo_revolution_theta_1(theta_1)
!    
!    ! Define varying vane length
!    call geo_vanelength(l_v, theta_1, r_oc, r_ro, r_vh, e)
!    
!    ! Define theta_2 & gamma_1
!    call geo_revolution_theta_2(theta_2, theta_1, l_v, r_oc, r_ro, r_vh, e)
!    call geo_comp_gamma_1(gamma_1, l_v, theta_1, r_oc, r_vh, r_ro, e)
!    
!    ! Define chamber volume changes
!    call geo_comp_volume(v_suc, v_com, theta_1, theta_2, l_v, gamma_1, l_com, r_oc, r_ro, r_vh, l_vh, e, w_v_ro,  w_vs_ro, l_vs_ro, w_vsro_gap, l_vsro_gap, w_vs_oil_path, h_vs_oil_path, l_vs_oil_path)
!    
!    ! Total volume
!    call geo_total_vol
!    
!2981 format (11A25)
!2982 format (F25.4, 14ES25.6) 
!end subroutine optimization_geo_m
!    
!    
!    
!subroutine optimization_kinematics_m(theta_1, gamma_1, l_v, dlvdt, dlvdtt, dgammadt_1, dgammadtt_1, dvdtheta1_suc, dvdtheta1_com)
!    
!    ! Simulation/operational parameters and constants
!    include "var_operational_parameter.f90"
!    include "var_simulation_parameter.f90"
!    include "var_physical_constants.f90"
!    
!    ! Geometrical model and main dimensions
!    include "var_main_dimensions.f90"
!    include "var_geometrical.f90"
!    
!    ! Kinematics and Differentiated Geometrical variables
!    include "var_kinematics.f90"
!    include "var_geometrical_diff.f90"
!    
!    ! Vane Velocity and Acceleration
!    call kine_vane(dlvdt, dlvdtt, dlvdtheta1, dlvdtheta12, theta_1)
!    
!    ! Rotor swinging movement
!    call kine_rotor(theta_1, gamma_1, l_v, dlvdt, dlvdtt, dgammadt_1, dgammadtt_1, dlvdtheta1, dlvdtheta12, dtheta2dt)
!    
!    ! Working chambers rate of change
!    call kine_working_chamber(theta_1, dlvdtheta1, dtheta2dt, dvdtheta1_suc, dvdtheta1_com) 
!    
!2985 format (11A25)
!2986 format (F25.4, 10ES25.6) 
!
!endsubroutine optimization_kinematics_m
!    
!    
!    
!    
!subroutine optimization_thermo_id(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id)
!    
!    ! REFPROP variables
!    implicit double precision (a-h,o-z)
!    implicit integer (i-k,m,n)
!    character(255) :: fluid, herr, hfmix
!    character(3) :: hrf
!    Integer, parameter :: nc = 1, ncmax = 20
!    dimension z(ncmax),x(ncmax),y(ncmax)
!    common/fluid_info/wm
!    
!    ! Main program variables
!    include "var_operational_parameter.f90"
!    include "var_operating_fluid_condition.f90"
!    include "var_simulation_parameter.f90"
!    include "var_physical_constants.f90"
!    include "var_compressor_evaluation.f90"
!    
!    double precision, dimension (1:no_data+1) :: v_suc, v_com, theta_1
!    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id, t_cscv_id, t_cdcv_id, m_cscv_id, m_cdcv_id, rho_cdcv_id
!    
!    ! -------------------------------------
!    ! Suction process (Pressure)
!    ! -------------------------------------
!    do 261 i = 1, no_data+1
!        t_cscv_id(i) = t_suc
!        p_cscv_id(i) = p_suc       ! suction Pressure
!        m_cscv_id(i) = rho_suc * v_suc(i)*1.d-9     ! ideal suction mass [kg]
!        
!261  continue
!    
!    !m_cdcv_id(1) = rho_suc * vol_total * 1.d-9       ! total mass [kg]
!    m_cdcv_id(1) = m_cscv_id(no_data+1)       ! total mass [kg]
!    
!    ! Compression process of 1st compressor (Pressure and Temperature changing)
!    do 262 i = 1, no_data+1
!        rho_cdcv_id(i) = m_cdcv_id(1)/(v_com(i)*1.d-9)    ! Density change
!        m_cdcv_id(i) = m_cdcv_id(1)                         ! Mass remain, no mass discharge before reaching disc. pressure
!        D = rho_cdcv_id(i)/wm                               ! Assign density (mol/L)
!        s = s_suc/1000 * wm                                ! Isentropy process (so entropy is the same after compressed) (J/(mol.K))
!        call DSFLSH (D,s,z,t,p,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)    ! calculate temperature and pressure 
!        
!        p_cdcv_id(i) = p
!        t_cdcv_id(i) = t
!        
!        if (p_cdcv_id(i) > p_disc)  then
!            p_cdcv_id(i) = p_disc
!            t_cdcv_id(i) = t_disc
!            m_cdcv_id(i) = m_cdcv_id(i)
!            rho_cdcv_id(i) = rho_cdcv_id(i)
!            exit
!        endif
!        
!262 continue
!        
!        if (p_cdcv_id(i) == p_disc) then
!            do 263 i = i, no_data+1
!                rho_cdcv_id(i) = rho_cdcv_id(i-1)
!                p_cdcv_id(i) = p_disc
!                t_cdcv_id(i) = t_disc
!                m_cdcv_id(i) = m_cdcv_id(i-1) - rho_cdcv_id(i-1)*(v_com(i-1)-v_com(i))*1.d-9
!263         continue
!        endif
!        
!! ------------------ Before Evaporator ------------------- !
!    j = 1
!    call TPRHO (t_high,p_disc,x,j,0,d,ierr,herr)
!    call THERM (t_high,d,x,p,e,h,s,cv,cp,w,hjt)
!    !call TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
!    
!    h_bef_evap = h/wm*1000              ! [J/kg.K] throttle valve suction H = discharge H (before evaporator)
!
!        
!        
!        m_flow_total_id = m_cdcv_id(1)*freq
!        q_capacity_id = m_flow_total_id * (h_suc - h_bef_evap)
!        !write(41,2981) 'Mass_flow_rate_total_id[kg]'
!        !write(41,2983) m_flow_total_id
!        !write (45,*) " -------------------------------------------------------------------- "
!        !write(45,*)  ' Ideal Overview  '
!        !write (45,*) " -------------------------------------------------------------------- "
!        !write(45,2990) 'Total Ideal Mass Flow Rate                m_flow_total_id        = ', m_flow_total_id, ' kg/s'
!        !write(45,2990) 'Ideal Cooling Capacity                    q_capacity_id          = ', q_capacity_id, ' W'
!        
!        
!2981 format (16A35)
!2982 format (F35.4, 14ES35.6) 
!2983 format (14ES35.6)  
!2989 format (2x,A,F8.4,A)
!2990 format (2x,A,f15.4,A)
!end subroutine optimization_thermo_id
!    
!subroutine optimization_thermo_m(theta_1, theta_2, dlvdt, dgammadt_1, v_com, v_suc, l_v, p_cscv_id, p_cdcv_id, dvdtheta1_s, dvdtheta1_d, dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
!    implicit none
!    integer, parameter :: k_stop = 3
!    integer i
!    include "var_operational_parameter.f90"
!    include "var_physical_constants.f90"
!    include "var_simulation_parameter.f90"
!    include "var_main_dimensions.f90"
!    include "var_geometrical.f90"
!    include "var_geometrical_diff.f90"
!    include "var_compressor_evaluation.f90"
!    include "var_thermo.f90"
!    include "var_thermo_error_criterion.f90"
!    include "var_thermo_error_ptr.f90"
!    include "var_indicated_work.f90"
!    include "var_reed_valve.f90"
!    double precision, dimension (1:no_data+1) :: dlvdt, dgammadt_1
!    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id
!    double precision, dimension (1:no_data+1) :: y_start, y_mid, y_end
!    ! --- Variables for Leakage --- !
!    double precision, dimension (1:no_data+1) :: dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d
!    double precision, dimension (1:no_data+1) :: dmdtheta1_leak_rad_ro, dmdtheta1_leak_vef, dmdtheta1_leak_vs, dmdtheta1_leak_ref
!    ! --- Total Heat Transfer in various chambers
!    double precision, dimension (1:no_data+1) :: dqdtheta_sc, dqdtheta_dc, dqdtheta_hc, dqdtheta_resoil, dqdtheta_oil
!    double precision, dimension (1:no_data+1) :: T_ocsc, T_ocdc, T_rosc, T_rodc, T_roller, T_uocsc, T_locsc, T_uocdc, T_locdc
!    double precision, dimension (1:no_data+1) :: T_ocsc2, T_ocsc3, T_ocdc2, T_ocdc3, T_rosc2, T_rosc3, T_rodc2, T_rodc3, T_roller2, T_roller3
!    double precision, dimension (1:no_data+1) :: Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex       ! for exergy calculation
!    integer k, journal_opt, heat_opt, leakage_opt
!    common/model_opt/journal_opt, heat_opt, leakage_opt
!    
!    ! ------------------------ Runge Kutta Coefficients ------------------------ !
!    !call Runge_steps_coefficients       ! runge-kutta step and coefficients
!    
!    ! ------------------------ Valve parameters & mode shape ------------------- !
!    call discharge_valve_parameters(no_data, A_cross_dv_1, I_dv_1, beta_r_1)
!    call discharge_valve_parameters_ver2(w_dv, t_dv, l_dv_ef, w_dv_x2, r_dv_x1, l_dv_x2, l_dv_x3, omega_natural_dv, omega_natural_dv_back)
!    !call discharge_valve_mode_shape(beta_r_1, x_hole_start_1, l_dv_1, dia_disc1, f_start_1, f_mid_1, f_end_1, integral_f_1)
!    call discharge_valve_mode_shape_ver2(t_dv, w_dv_x2, w_dv, l_dv_ef, r_dv_x1, l_dv_x2, l_dv_x3, dia_disc, F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start_dv, phi_mode_mid_dv, phi_mode_end_dv, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back)
!    
!    ! ===========================================================================
!    ! Thermodynamic with heat transfer and leakage
!    ! re-expansion model included
!    ! --------------------------------------------------------------------------
!    ! Declare Error Parameter
!    ! Initialise : k, error evaluation parameters
!    ! --------------------------------------------------------------------------
!    k = 1
!    
!    !------- Outer comp ------- !
!    error_pos = error_default       ! *100 to be percentage (100.0% as initial) 
!    error_poc = error_default       !error_default = 1.0
!    error_tos = error_default
!    error_toc = error_default
!    ! -----------------------------------------
!    ! Initialise residual mass and enthalpy
!    ! residual mass is caused by dead volume
!    ! -----------------------------------------
!    print *, ' '
!    print *, ' >>>>> Thermodynamic (Actual): With Heat Transfer and Leakage '
!    print *, ' '
!    mass_residual_dead = 0.0
!    h_residual_dead = 0.0
!    
!    ! ------------------------------
!    ! initialise leakage and heat transfer variables
!    ! ------------------------------
!    do i = 1, no_data+1
!        ! ----- Leakage variables ------ !
!        dmdtheta1_leak_s(i) = 0.d0
!        dedtheta1_leak_s(i) = 0.d0
!        dmdtheta1_leak_d(i) = 0.d0
!        dedtheta1_leak_d(i) = 0.d0
!        dmdtheta1_leak_rad_ro(i) = 0.d0
!        dmdtheta1_leak_vef(i) = 0.d0
!        dmdtheta1_leak_vs(i) = 0.d0
!        
!        ! ----- Heat Transfer variables ----- !
!        dqdtheta_sc(i) = 0.d0
!        dqdtheta_dc(i) = 0.d0
!        dqdtheta_hc(i) = 0.d0
!        dqdtheta_resoil(i) = 0.d0
!        dqdtheta_oil(i) = 0.d0
!    enddo
!    
!    do while (abs(error_pos) > conv_criterion .or. abs(error_poc) > conv_criterion .or. abs(error_tos) > conv_criterion .or. abs(error_toc) > conv_criterion)
!        
!        print *, ' ---------------------------------------------------------------------------- '
!        print '(2x,A,I3)', ' Iteration ',k
!        print *, ' '
!        
!        ! ------------------------ Thermodynamics Properties (Re-expansion/residual mass + internal leakage + heat transfer) ------------------------------ !
!        call thermo_with_heat_leakage(theta_1, v_com, v_suc, dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_di, dmdtheta1_do, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dmdtheta1_s, dedtheta1_s, dmdtheta1_d, dedtheta1_d, dvdtheta1_s, dvdtheta1_d, y_start, y_mid, y_end)
!        
!        ! ------------------------ Leakage Model ----------------------------------- !
!        if (leakage_opt == 1) then
!            print *, ' Running Leakage Model ... '
!            call leakage_m(theta_1, p_scv, t_scv, p_dcv, t_dcv, cv_dcv, cp_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, l_v, dmdtheta1_leak_rad_ro, dmdtheta1_leak_vef, dmdtheta1_leak_vs, dmdtheta1_leak_ref, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d)
!        endif
!        
!        ! ------------------------ Heat Transfer Model ----------------------------------- !
!        if (heat_opt == 1) then
!            print *, ' Running Heat Transfer Model ... '
!            call heat_transfer_m(theta_1, theta_2, v_com, v_suc, dlvdt, dgammadt_1, l_v, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dqdtheta_sc, dqdtheta_dc, dqdtheta_hc, dqdtheta_resoil, dqdtheta_oil, T_ocsc, T_ocdc, T_rosc, T_rodc, T_roller, T_uocsc, T_locsc, T_uocdc, T_locdc, T_ocsc2, T_ocsc3, T_ocdc2, T_ocdc3, T_rosc2, T_rosc3, T_rodc2, T_rodc3, T_roller2, T_roller3, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
!        endif
!        ! ------------------------ Error checking ---------------------------------- !
!        call thermo_error_check(k, k_stop, p_scv, p_dcv, t_scv, t_dcv, p_test_suct, p_test_comp, t_test_suct, t_test_comp)
!        
!        k = k + 1
!        if (k > k_stop) then
!            print *, ' End of Thermodynamic (Actual): With heat transfer and leakage '
!            print *, ' ---------------------------------------------------------------------------- '
!            print *, ' '
!            exit
!        endif
!        print *, ' '
!        print *, ' '
!        print *, ' '
!    end do
!
!   
!    
!    
!    
!    ! ----------------------------------------------------------------------------!
!    ! Data writing !
!    ! NVC Thermodynamics properties (with heat transfer and internal leakage)
!    ! ----------------------------------------------------------------------------!
!!    
!!    write (77,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
!!    write (76,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
!!    write (75,2981), "Degree","Volume[m3]","Pressure[kPa]","Temperature[K]","Int. Energy[J/kg]","Density[kg/m3]","Enthalpy[J/kg]","Entropy[J/kg.K]","Dyn. Viscosity[N.s/m^2]","Spec. Heat[J/kg.K]","Therm. Cond[W/(m.K)]","Mass[kg]", "Valve Deflec. mid[mm]", "Valve Deflec. tip[mm]"
!!    write (78,2981), "Volume[m3]", "Pressure[kPa]"
!!    write (81,2981), "Degree", "dmdtheta1", "dedtheta1", "dvdtheta1"
!!    !write (70,2981), "Degree", "dmdtheta1_s", "dedtheta1_s", "dvdtheta1_s"
!!    !write (79,2981), "Degree", "dmdtheta1_d", "dedtheta1_d", "dvdtheta1_d"
!!    !write (82,2981), "Degree", "y_start[mm]", "y_stop[mm]"
!!    write (83,2981), "Degree", "y_mid[mm]", "y_start[mm]"
!!    write (95,2981), "Degree", "Press.Ratio", "Mass_leak_rad_ro[kg/s]", "Mass_leak_vef[kg/s]", "Mass_leak_vs[kg/s]", "Mass_leak_ref[kg/s]", "Mass_leak_total[kg/s]"
!!    write (96,2981), "Degree", "dqdtheta_sc[W]", "dqdtheta_dc[W]", "dqdtheta_hc[W]", "dqdtheta_resoil[W]", "dqdtheta_oil[W]"
!!    write (97,2981), "Degree", "T_ocsc[K]", "T_ocdc[K]", "T_rosc[K]", "T_rodc[K]", "T_roller[K]", "T_uocsc[K]", "T_locsc[K]", "T_uocdc[K]", "T_locdc[K]"
!!    write (98,2981), "Degree", "T_ocsc2[K]", "T_ocsc3[K]", "T_ocdc2[K]", "T_ocdc3[K]", "T_rosc2[K]", "T_rosc3[K]", "T_rodc2[K]", "T_rodc3[K]", "T_roller2[K]", "T_roller3[K]"
!!
!!    ! ----------------------------------------------------------------------------!
!!    ! NVC Compressor Suction
!!    ! ----------------------------------------------------------------------------!
!!    !do 291 i = 1, max_write_data, data_step_suct
!!    do 293 i = 1, no_data+data_step_suct, data_step_suct
!!        write(81,2982) theta_1(i)*180./pi, dmdtheta1_s(i),dedtheta1_s(i), dvdtheta1_s(i)
!!        write(75,2982) theta_1(i)*180./pi, v_suc(i)*1.d-9,p_scv(i), t_scv(i), u_scv(i), rho_scv(i),h_scv(i), s_scv(i), miu_scv(i), cp_scv(i), k_scv(i), m_scv(i), y_mid(1)*1000., y_start(1)*1000.
!!        write(78,2983) v_suc(i)*1.d-9, p_scv(i)
!!        write(77,2982) theta_1(i)*180./pi, v_suc(i)*1.d-9,p_scv(i), t_scv(i), u_scv(i), rho_scv(i), h_scv(i), s_scv(i), miu_scv(i), cp_scv(i), k_scv(i), m_scv(i), y_mid(1)*1000., y_start(1)*1000.
!!        !write(82,2982) theta_1(i)*180./pi, y_start_1(i)*1000., y_end_1(i)*1000.
!!        write(83,2982) theta_1(i)*180./pi, y_mid(1)*1000., y_start(1)*1000.
!!        
!!293 continue   
!!    ! ----------------------------------------------------------------------------!
!!    ! NVC Compressor compression (discharge)
!!    ! ----------------------------------------------------------------------------!
!!    !do 292 i = 1, max_write_data, data_step
!!    do 294 i = 1, no_data+data_step, data_step
!!        !write(70,2982) theta_1(i)*180./pi, dmdtheta1_s_1(i),dedtheta1_s_1(i), dvdtheta1_s_1(i)
!!        write(81,2982) 360.+theta_1(i)*180./pi, dmdtheta1_d(i), dedtheta1_d(i),dvdtheta1_d(i)
!!        write(75,2982) 360.+theta_1(i)*180./pi, v_com(i)*1.d-9, p_dcv(i), t_dcv(i), u_dcv(i), rho_dcv(i),h_dcv(i), s_dcv(i), miu_dcv(i), cp_dcv(i), k_dcv(i), m_dcv(i), y_mid(i)*1000., y_start(i)*1000.
!!        write(76,2982) theta_1(i)*180./pi, v_com(i)*1.d-9, p_dcv(i), t_dcv(i), u_dcv(i), rho_dcv(i), h_dcv(i), s_dcv(i), miu_dcv(i), cp_dcv(i), k_dcv(i), m_dcv(i), y_mid(i)*1000., y_start(i)*1000.
!!        write(78,2983) v_com(i)*1.d-9, p_dcv(i)
!!        !write(82,2982) 360+theta_1(i)*180/pi, y_start_1(i)*1000., y_end_1(i)*1000.
!!        write(83,2982) 360.+theta_1(i)*180./pi, y_mid(i)*1000., y_start(i)*1000.
!!        ! write data into test files  below
!!        
!!294 continue 
!!    ! ------------------------------------------------------------------ !
!!    ! Leakage and Heat Transfer data writing
!!    ! ------------------------------------------------------------------ !
!!    do 295 i = 1, no_data+data_step, data_step
!!        write(95,2982) theta_1(i)*180./pi, p_dcv(i)/p_scv(i), dmdtheta1_leak_rad_ro(i)*omega_1, dmdtheta1_leak_vef(i)*omega_1, dmdtheta1_leak_vs(i)*omega_1, dmdtheta1_leak_ref(i)*omega_1, dmdtheta1_leak_s(i)*omega_1
!!        write(96,2982) theta_1(i)*180./pi, dqdtheta_sc(i)*omega_1, dqdtheta_dc(i)*omega_1, dqdtheta_hc(i)*omega_1, dqdtheta_resoil(i)*omega_1, dqdtheta_oil(i)*omega_1
!!        write(97,2982) theta_1(i)*180./pi, T_ocsc(i), T_ocdc(i), T_rosc(i), T_rodc(i), T_roller(i), T_uocsc(i), T_locsc(i), T_uocdc(i), T_locdc(i)
!!        write(98,2982) theta_1(i)*180./pi, T_ocsc2(i), T_ocsc3(i), T_ocdc2(i), T_ocdc3(i), T_rosc2(i), T_rosc3(i), T_rodc2(i), T_rodc3(i), T_roller2(i), T_roller3(i)
!!295 continue 
!    ! non-deal mass flow rate
!    
!    !write(42,2981) 'Mass_Flow_1[kg/s]', 'Mass_Flow_2[kg/s]', 'Mass_Flow_Total[kg/s]', ' Inlet Spec. Enthalpy[J/kg]'
!    !write(42,2983) mass_outer, mass_inner, mass_total, h_bef_evap
!    
!    
!2981 format (16A25)
!2982 format (F25.4, 14ES25.6)    
!2983 format (14ES25.6) 
!2989 format (2x,A,F8.4,A)
!endsubroutine optimization_thermo_m
!
!    
!subroutine optimization_indicated_work(v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, dvdtheta1_s, dvdtheta1_d, power_ind, power_ind_ideal, power_loss_suc, power_loss_disc, power_comp_only)
!    implicit none
!    include 'var_operational_parameter.f90'
!    include 'var_operating_fluid_condition.f90'
!    include 'var_simulation_parameter.f90'
!    double precision power_ind, power_ind_ideal, power_loss_suc, power_loss_disc, power_comp_only
!
!    !real P_ind_id_1, P_ind_id_2, P_ind_1, P_ind_2, P_loss_suc_1, P_loss_suc_2, P_loss_disc_1, P_loss_disc_2
!    double precision, dimension (1:no_data+1) :: w_suc_id, w_disc_id, w_disc_all_id, w_suc, w_disc, w_disc_diff
!    double precision, dimension (1:no_data+1) :: p_scv, p_dcv, v_suc, v_com
!    double precision, dimension (1:no_data+1) :: dvdtheta1_s, dvdtheta1_d
!    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id
!    integer i
!    
!    ! ------------------------------------------------- 
!    ! Calculate Indicated Work
!    ! W_ind = Pressure * dV/d@ * omega_1
!    ! or W_ind = Pressure*(change in Volume)*frequency
!    ! ------------------------------------------------- 
!    
!    do 288 i = 1, no_data+1
!        
!        w_suc_id(i) = p_cscv_id(i)*dvdtheta1_s(i)*1000.0*omega_1 !0.5*(p_cscv_id(i) + p_cscv_id(i+1))*dvdtheta1_s(i)*1000.0*omega_1
!        w_suc(i) = p_scv(i)*dvdtheta1_s(i)*1000.0*omega_1 !0.5*(p_scv(i) + p_scv(i+1))*dvdtheta1_s(i)*1000.0*omega_1            ! work done during suction process
!            
!        w_disc(i) = - p_dcv(i)*dvdtheta1_d(i)*1000.0*omega_1 !0.5*(p_dcv(i) + p_dcv(i+1))*dvdtheta1_d(i)*1000.0*omega_1         ! work done in discharge process (volume decreasing so using -ve)
!        w_disc_all_id(i) = - p_cdcv_id(i)*dvdtheta1_d(i)*1000.0*omega_1 !0.5*(p_cdcv_id(i) + p_cdcv_id(i+1))*dvdtheta1_d(i)*1000.0*omega_1
!        
!        if (p_dcv(i) < p_disc) then
!            w_disc_diff(i) = 0.0
!            w_disc_id(i) = 0.0
!        else
!            !w_disc_diff_1(i) = - p_dcv_1(i)*(v_com1(i+1)-v_com1(i))*1000.0*1.d-9*freq  
!            !w_disc_id_1(i) = -p_cdcv_id_1(i)*(v_com1(i+1)-v_com1(i))*1000.0*1.d-9*freq
!            !w_disc_diff(i) = -0.5*(p_dcv(i) + p_dcv(i+1))*dvdtheta1_d(i)*1000.0*omega_1 
!            !w_disc_id(i) = -0.5*(p_cdcv_id(i) + p_cdcv_id(i+1))*dvdtheta1_d(i)*1000.0*omega_1
!            w_disc_diff(i) = -p_dcv(i)*dvdtheta1_d(i)*1000.0*omega_1 
!            w_disc_id(i) = -p_cdcv_id(i)*dvdtheta1_d(i)*1000.0*omega_1
!        endif
!
!288 continue 
!        
!        power_ind_ideal = (- sum(w_suc_id) + sum(w_disc_all_id))/(no_data+1)
!        power_ind = (- sum(w_suc) + sum(w_disc))/(no_data+1)
!        power_loss_suc = (sum(w_suc_id) - sum(w_suc))/(no_data+1)
!        power_loss_disc = (sum(w_disc_diff) - sum(w_disc_id))/(no_data+1)
!        power_comp_only = (sum(w_disc) - sum(w_disc_all_id))/(no_data+1)
!
!        
!2981 format (14A25)
!2982 format (F25.4, 14ES25.6)
!2983 format (14ES25.6)  
!2989 format (2x,A,F8.4,A)
!2990 format (2x,A,f15.4,A)
!endsubroutine optimization_indicated_work
!    
!    
!    
!    
!subroutine optimization_power_m(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, h_scv, h_dcv, dmdtheta1_si, dmdtheta1_so, dmdtheta1_di, dmdtheta1_do, dvdtheta1_s, dvdtheta1_d, dlvdt, dgammadt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, eccr, att, T_inertia_ro, T_inertia_vh, T_com_C, L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s)
!    implicit none
!    include 'var_simulation_parameter.f90'
!    include "var_operational_parameter.f90"
!    include 'var_physical_constants.f90'
!    include 'var_main_dimensions.f90'
!    include 'var_kinematics.f90'
!    include "var_geometrical.f90"
!    include "var_geometrical_diff.f90"
!    include 'var_compressor_evaluation.f90'
!    include "var_dynamic.f90"
!    include "var_clearance.f90"
!    include "var_indicated_work.f90"
!    include "var_operating_fluid_condition.f90"
!    ! ---------- for optimization ----------- !
!    include "var_optimization.f90"
!    ! ---------- for simulation ------------- !
!    integer i, journal_opt, heat_opt, leakage_opt
!    common/model_opt/journal_opt, heat_opt, leakage_opt
!    integer optimization_opt
!    common/routine_opt/optimization_opt
!    ! ---------- input variables ------------ !
!    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id   ! suction and compression chamber pressure (ideal)
!    double precision, dimension (1:no_data+1) :: p_scv, p_dcv, h_scv, h_dcv   ! suction chamber and compression chamber properties (actual)
!    double precision, dimension (1:no_data+1) :: dmdtheta1_si, dmdtheta1_so, dmdtheta1_di, dmdtheta1_do
!    double precision, dimension (1:no_data+1) :: eccr, att      ! journal bearing
!    ! ---------- Output variables are in the file var_power.f90 ---------- !
!    include "var_power.f90"
!    ! ---------- dummy variables ------------ !
!    double precision L_ef_vh_I, L_ef_vh_II, A_ratio_vh
!    double precision vel_avrg
!    
!    
!    ! -------------------------------------------
!    ! Indicated Power (with heat transfer and leakage model)
!    ! -------------------------------------------
!    call optimization_indicated_work(v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, dvdtheta1_s, dvdtheta1_d, P_ind_hl_opt, P_ind_id_opt, P_loss_suc_hl_opt, P_loss_disc_hl_opt, P_comp_loss_hl)
!    
!    ! -------------------------------------------
!    ! valve loss 
!    ! -------------------------------------------
!    P_valve_loss_opt = P_loss_suc_hl_opt + P_loss_disc_hl_opt       ! total valve loss due to discharge and suction
!    ! ----- Dummy calculation -----------
!    A_ratio_vh = 1 - (l_v_ro + l_vs_ro)*w_v_ro/(pi*r_vh**2 + (2*w_vs_ro + w_v_ro)*l_vh)     ! Area ratio for vane housing
!    vel_avrg = r_roi*1.e-3*omega_1   ! velocity [m/s], velocity calculate to the tip of the roller, and the other side
!    !T_lub = 2.*pi*(r_ecc*1.e-3)*(l_ecc*1.e-3)*miu_oil*vel_avrg/cl_rad_roll*r_ecc*1.e-3        ! [Nm] Area*Stress*moment_arm, assume constans moment arm, torque due to lubricant dragging friction
!    
!    do 198 i = 1, no_data+1
!        ! -----------------------------------------------------------------
!        ! Total 5 kinds of frictional losses, 
!        ! L_f_vs -- vane side friction
!        ! L_ef_vh -- vane housing endface friction
!        ! L_s_vh -- vane housing side friction
!        ! L_ef_ro -- rotor endface
!        ! L_lub -- lubricant layer friction (roller and rotor)
!        ! ------------ Vane housing friction (endface and side ) ---------- !
!        L_ef_vh_I = pi*miu_oil*(r_vh*1.e-3)**4/cl_upend*dgammadt_1(i)**2      ! [W] Dummy -- frictional loss of vane housing Section I
!        L_ef_vh_II = 2*pi*miu_oil*(l_vh*1.e-3)/cl_upend*(2*w_vs_ro + w_v_ro)*1.e-3*((r_vh + 0.5*l_vh)*1.e-3)**2*dgammadt_1(i)**2        ! [W] Dummy -- frictional loss of vane housing Section II
!        L_ef_vh(i) = A_ratio_vh*(L_ef_vh_I + L_ef_vh_II)            ! [W] endface frictional loss of vane housing 
!        L_ef_vh(i) = abs(L_ef_vh(i))                ! [W] only absolute value
!        L_s_vh(i) = r_vh*1.e-3*coef_vhs*sqrt(F_vhoc_n(i)**2 + F_vhoc_t(i)**2)*dgammadt_1(i)     ! [W] vane housing side frictional loss
!        L_s_vh(i) = abs(L_s_vh(i))       ! [W]  take only absolute value
!        ! ------------ Vane side friction ------------------- !
!        L_f_vs(i) = abs(coef_vs*(F_1_n(i)) + (F_2_n(i))*dlvdt(i)*1.e-3)      ! [W] vane side frictional loss (abs)
!        ! ------------ Rotor friction ----------------------- !
!        L_ef_vro(i) = 2*miu_oil/cl_upend*((l_v_ro*w_v_ro*1.e-6*(dlvdt(i)*1.e-3)**2) + (l_v_ro*w_v_ro*1.e-6*((r_vh + l_v(i) - 0.5*l_v_ro)*1.e-3)**2*dgammadt_1(i)**2))   ! [W]  rotor vane endface frictional loss
!        L_ef_vro(i) = abs(L_ef_vro(i))
!        L_ef_rohc(i) = 2*pi*miu_oil/cl_upend*((r_ro**2 - r_roi**2)*dlvdt(i)**2*1.e-12 + (0.5*(r_ro**4 - r_roi**4) + (r_vh + l_v(i) + r_ro)**2*(r_ro**2 - r_roi**2))*1.e-12*dgammadt_1(i)**2)
!        L_ef_rohc(i) = abs(L_ef_rohc(i))        ! [W] rotor body (cylinder) endface frictional loss
!        L_ef_ro(i) = L_ef_vro(i) + L_ef_rohc(i)         ! Rotor total endface frictional loss
!        ! ------------ Friction between roller and rotor ------------- !
!        !L_lub(i) = abs(T_lub*omega_1)       ! [W] frictional loss due to rubbing surface between roller and rotor
!        T_ecc(i) = miu_oil*(omega_1 - dgammadt_1(i))*r_ecc**3*l_ecc*1.e-12*pi*( (2.0 + eccr(i)) / (1.0 + eccr(i)) ) / ( cl_rad_roll*sqrt(1.0-eccr(i)**2))  +  cl_rad_roll*eccr(i)*F_resultant(i)*sin(att(i))/2.    ! [Nm] journal bearing analysis
!        L_lub(i) = abs(T_ecc(i)*(omega_1-dgammadt_1(i)))       ! [W] frictional loss due to rubbing surface between roller and rotor
!        ! ===================================================
!        ! Total Instantaneous power, total frictional power loss, 
!        ! rotor inertia, vane housing inertia, compression power (individual)
!        ! calcualte power into the system first
!        ! then move the centre to C
!        ! ------------------------------------------------------
!        if (journal_opt == 1) then
!            P_bear_s(i) = r_bearing*1.e-3*omega_1*(miu_oil*omega_1*r_bearing**2*l_bearing*1.e-9*pi*( (2.0 + eccr(i)) / (1.0 + eccr(i)) ) / ( cl_bear_s*sqrt(1.0-eccr(i)**2)))  +  r_bearing*1.e-3*omega_1*(cl_bear_s*eccr(i)*F_resultant(i)*sin(att(i))/(2*r_bearing*1.e-3))
!        else
!            P_bear_s(i) = 0.0
!        endif
!        P_f_loss(i) = L_ef_vh(i) + L_s_vh(i) + L_f_vs(i) + L_ef_ro(i) + L_lub(i) + P_bear_s(i)     ! [W]    Instantaneous total frictional power loss
!        !P_thr_loss(i) = (abs((dmdtheta1_si(i) - dmdtheta1_so(i))*(h_suc - h_scv(i))) + abs((dmdtheta1_di(i) - dmdtheta1_do(i))*(h_dcv(i) - h_disc)))*omega_1  ! [W] throttling loss between suction port to chamber and chamber to discharge port
!        P_inertia_ro(i) = T_inertia_ro(i)*dgammadt_1(i)        ! [W] rotor inertia power
!        P_inertia_vh(i) = T_inertia_vh(i)*dgammadt_1(i)        ! [W] Vane housing inertia power
!        P_com(i) = T_com_C(i)*omega_1                    ! [W] Compression power
!        P_inst_total_no_loss(i) = P_inertia_ro(i) + P_inertia_vh(i) + P_com(i)  
!        !P_inst_total(i) = P_inertia_ro(i) + P_inertia_vh(i) + P_com(i) + P_f_loss(i) + P_valve_loss
!        
!        ! ---------- Reexpansion loss ------------- !
!        if (theta_1(i) > theta_disc_end .and. i .ne. no_data+1) then
!            L_reexpansion(i) = - 0.5*(p_dcv(i) + p_dcv(i+1))*dvdtheta1_d(i)*1000.0*omega_1      ! loss to compress the residual mass which is expanded to compression chamber in next cycle
!        else
!            L_reexpansion(i) = 0.0
!        endif
!        
!        ! -----------------------------------------
!        P_inst_total(i) = P_inertia_ro(i) + P_inertia_vh(i) + P_com(i) + P_f_loss(i) + P_valve_loss + L_reexpansion(i)
!        ! --------- friction torque -------------- !
!        T_loss(i) = P_f_loss(i)/omega_1
!        ! --------- Torque of bearing friction --- !
!        T_bear_s(i) = P_bear_s(i)/omega_1
!        ! --------- Inertia torque (about C) ----- !
!        T_inertia_ro(i) = P_inertia_ro(i)/omega_1
!        T_inertia_vh(i) = P_inertia_vh(i)/omega_1
!        
!        ! --------- Compression torque (about C) -- !
!        T_com_C(i) = P_com(i)/omega_1
!        
!        ! --------- Total torque without loss ----- !
!        T_total_no_loss(i) = P_inst_total_no_loss(i) / omega_1
!        
!        ! --------- Total Instantaneous torque ---- !
!        T_inst_total(i) = P_inst_total(i)/omega_1
!198 continue        
!        ! ---------------- Average individual loss ------------- !
!        L_avrg_ef_vh_opt = sum(L_ef_vh)/(no_data+1)
!        L_avrg_s_vh_opt = sum(L_s_vh)/(no_data+1)
!        L_avrg_f_vs_opt = sum(L_f_vs)/(no_data+1)
!        L_avrg_ef_ro_opt = sum(L_ef_ro)/(no_data+1)
!        L_avrg_lub_opt = sum(L_lub)/(no_data+1)
!        L_avrg_reexpansion_opt = sum(L_reexpansion)/(no_data+1)
!        ! ---------------- Average power/power loss ------------ !
!        P_input_opt = sum(P_inst_total)/(no_data+1)     ! [W] average total instantaneous power, which is also input power (loss included)
!        P_avrg_no_loss_opt = sum(P_inst_total_no_loss)/(no_data+1)      ! [W] Average instantaneous power without loss
!        P_avrg_loss_opt = sum(P_f_loss)/(no_data+1)     ! [W] total average loss (friction)
!        P_avrg_inertia_ro_opt = sum(P_inertia_ro)/(no_data+1)           ! [W] Average inertial power or rotor
!        P_avrg_inertia_vh_opt = sum(P_inertia_vh)/(no_data+1)           ! [W] Average inertia power of vane housing
!        P_avrg_com_opt = sum(P_com)/(no_data+1)                         ! [W] Average Compression power
!        P_avrg_bear_s_opt = sum(P_bear_s)/(no_data+1)
!        P_avrg_ecc_opt = L_avrg_lub_opt     ! [W] Average eccentric frictional loss
!        !P_avrg_thr_loss = sum(P_thr_loss)/(no_data+1)           ! [W] Average throttling loss
!        ! ----------------- Average Torque ------------- !
!        T_avrg_no_loss = P_avrg_no_loss/omega_1     ! [Nm] Average torque when no losses
!        T_avrg_input = P_input/omega_1              ! [Nm] Average input torque
!        T_avrg_loss = P_avrg_loss/omega_1           ! [Nm] average torque of frictional losses
!        T_avrg_I_ro = P_avrg_inertia_ro/omega_1     ! [Nm] average rotor inertia torque
!        T_avrg_I_vh = P_avrg_inertia_vh/omega_1     ! [Nm] average vane housing inertia torque
!        T_avrg_com = P_avrg_com/omega_1             ! [Nm] average compression torque
!        T_avrg_bear_s = P_avrg_bear_s/omega_1       ! [Nm] average bearing friction torque
!        T_peak_opt = maxval(P_inst_total)/omega_1       ! [Nm] Maximum shaft torque
!        
!        ! -----------------------------------------------------------------------------
!        ! write data into file "Forces - resultant.txt" & "Torques - individual components.txt"
!!        ! -----------------------------------------------------------------------------
!!        write (90,2981), "Degree","F_vhoc_n[N]","F_vhoc_t[N]","F_1_n[N]","F_2_n[N]","F_cx[N]","F_cy[N]", "F_resultant[N]"    ! version 2.3
!!        write (91,2981), "Degree","T_inertia_ro[Nm]", "T_inertia_vh[Nm]", "T_com_C[Nm]", "T_total_no_loss[Nm]", "T_loss[Nm]", "T_bear_s[Nm]", "T_inst_total[Nm]"
!!        write (92,2981), "Degree","L_f_vs[W]", "L_ef_vh[W]", "L_s_vh[W]", "L_ef_ro[W]", "L_lub[W]", "L_ef_vro[W]", "L_ef_rohc[W]"
!!        write (93,2981), "Degree","P_inst_total[W]", "P_inst_total_no_loss[W]", "P_f_loss[W]", "P_inertia_ro[W]", "P_inertia_vh[W]", "P_com[W]", "P_bear_s[W]" 
!!    do 124 i = 1, no_data+data_step, data_step
!!        ! -------------------------------------- !
!!        ! Create header for txt file
!!        ! -------------------------------------- !
!!        
!!        !write(90,2982) theta_1(i)*180/pi, F_ox(i), F_oy(i), T_vhro(i), M_o(i), F_1_n(i), F_2_n(i), F_cx(i), F_cy(i), T_roll(i)     ! for 9 unknown case only
!!        write(90,2982) theta_1(i)*180/pi, F_vhoc_n(i), F_vhoc_t(i), F_1_n(i), F_2_n(i), F_cx(i), F_cy(i), F_resultant(i)     ! for 6 unknown case only
!!        write(91,2982) theta_1(i)*180/pi, T_inertia_ro(i), T_inertia_vh(i), T_com_C(i), T_total_no_loss(i), T_loss(i), T_bear_s(i), T_inst_total(i)
!!        write(92,2982) theta_1(i)*180/pi, L_f_vs(i), L_ef_vh(i), L_s_vh(i), L_ef_ro(i), L_lub(i), L_ef_vro(i), L_ef_rohc(i)
!!        write(93,2982) theta_1(i)*180/pi, P_inst_total(i), P_inst_total_no_loss(i), P_f_loss(i), P_inertia_ro(i), P_inertia_vh(i), P_com(i), P_bear_s(i)
!!124 continue      
!
!    
!2981 format (15A25)
!2982 format (F25.4, 14ES25.6)    
!2983 format (14ES25.6) 
!endsubroutine optimization_power_m