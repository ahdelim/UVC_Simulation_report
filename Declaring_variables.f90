    ! ---------- Timing
    real count_rate
    Integer count, count_minute, count_total
    real CPU_start, CPU_Minute, CPU_condition, CPU_geo, CPU_kine, CPU_thermo_id, CPU_thermo, CPU_dynamic, CPU_journal, CPU_oil_lub, CPU_power, CPU_exergy, CPU_finish 
    
    real CPU_optimization, CPU_optimization_finish, CPU_optimization_GA, CPU_optimization_GA_end
    
    ! ---------- Refrigerant 
    character(255), dimension (1:20) :: fluid
    
    ! ---------- Simulation model options
    integer journal_opt, heat_opt, leakage_opt, oil_network_opt, exergy_opt
    integer optimization_opt, optimization_GA_opt
    common/routine_opt/optimization_opt, optimization_GA_opt
    common/model_opt/journal_opt, heat_opt, leakage_opt, oil_network_opt, exergy_opt
    
    
    ! ---------- Simulation parameter
    integer, parameter :: max_no_data = 200000
    include "var_simulation_parameter.f90"

    ! -------- Operational Parameters/Constant (coefficients, Angular vel) ----------
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    
    ! -------- Geomectrical Variables (Port Size, Main dimensions)------------
    include "var_Main_dimensions.f90"
    double precision, dimension (1:max_no_data+1) :: theta_1, theta_2                                 ! Revolution angles
    double precision, dimension (1:max_no_data+1) :: l_v                                                ! Length of vane exposed to the chamber
    double precision, dimension (1:max_no_data+1) :: v_com, v_suc                            ! Suction/Compression chamber volumes
    double precision, dimension (1:max_no_data+1) :: gamma_1                                                ! Subtended Angles
    
    ! -------------- Differentiated Variables
    double precision , dimension (1:max_no_data+1) :: dvdtheta1_suc, dvdtheta1_com      ! differentiated volume
    double precision , dimension (1:max_no_data+1) :: dvdtheta1_s, dvdtheta1_d          ! differentiated volume
    
    ! ------------  Kinematic Variables
    double precision, dimension (1:max_no_data+1) :: dgammadtheta1_1
    double precision, dimension (1:max_no_data+1) :: dgammadt_1, dgammadtt_1        ! gamma_1 speed and acceleration
    double precision, dimension (1:max_no_data+1) :: dlvdtheta1, dlvdtheta12        ! vane differentiated w.r.t theta_1
    double precision, dimension (1:max_no_data+1) :: dlvdt, dlvdtt      ! vane speed and acceleration
    
    
    ! ------------  Thermodynamic Variables Fluid conditions
    include "var_operating_fluid_condition.f90"
    
    ! ------------ Ideal Thermodynamic Variables
    double precision, dimension (1:max_no_data+1) :: p_cscv_id, p_cdcv_id        ! Ideal compressor suction/discharge pressure
    
    ! ------------ Thermodynamic Variables
    double precision, dimension (1:max_no_data+1) :: p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv
    double precision, dimension (1:max_no_data+1) :: p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv
    double precision, dimension (1:max_no_data+1) :: Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex       ! for exergy calculation
    double precision, dimension (1:max_no_data+1) :: dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do
    
    ! ------------ Dynamic Variables
    !double precision, dimension (1:max_no_data+1) :: F_ox, F_oy, T_vhro, M_o, F_1_n, F_2_n, F_cx, F_cy, T_roll       ! Forces to be determined
    double precision, dimension (1:max_no_data+1) :: F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant       ! Forces to be determined (for 6 unknowns)
    double precision, dimension (1:max_no_data+1) :: T_inertia_ro, T_inertia_vh, T_com_C, T_total_no_loss
    !double precision, dimension (1:max_no_data+1) :: F_1_n, F_2_n, T_vhoc, F_cx, F_cy, T_roll       ! Forces to be determined (for 6 unknowns)
    double precision, dimension (1:max_no_data+1) :: I_ro_O
    ! ------------ Frictional losses
    double precision, dimension (1:max_no_data+1) :: L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s, L_lip_seal       ! Losses definition
    ! ------------ Journal Bearing Variables
    double precision, dimension (1:max_no_data+1) :: eccr, att, h_min, p_max, Q_martin
    ! ------------ Compressor Performance Evaluation
    include "var_compressor_evaluation.f90"