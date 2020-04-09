    real CPU_opt_start
    integer obj_func_input, obj_var_set
    integer, parameter :: max_opt_reflect_no = 2000
    !double precision, parameter :: factor_a = 1.3, factor_b = 0.0001    ! reflection factor "alpha", and constraints boundary factor "beta"
    common/optimization_obj/obj_func_input, obj_var_set
    double precision factor_a, factor_b
    common/optimization_para_re/factor_a, factor_b
    
    integer opt_ori_complex, opt_i, opt_re, opt_idle_check, improv_check_cut_off, opt_cut_off
    common/optimization_para/opt_ori_complex, opt_i, opt_re, opt_idle_check, improv_check_cut_off, opt_cut_off
    
    double precision vol_min, vol_max, vol_max_2                                   ! Minimum and Maximum volume, maximum volume for set 2
    common/optimization_cond/vol_min, vol_max, vol_max_2
    
    double precision opt_conv_error         ! convergence error define as best - worst / best
    double precision, parameter :: opt_conv_cri = 0.0005     ! convergence criteria (0.001*100 = 0.1%)
    
    ! --- Geometrical variables
    double precision exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8            ! Explicit variables !  r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
    double precision exp1_2, exp2_2, exp3_2, exp4_2, exp5_2, exp6_2             ! Explicit variables ! r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv
    double precision geo1, geo2, geo3, geo4 ,geo5, geo6   ! Geometrical ! r_hc, r_ro, t_ro, l_v_ro, r_vh, e
    double precision geo1_2, geo2_2, geo3_2, geo4_2   ! Geometrical ! r_hc, l_com, e
    !common/optimization_variables/exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, geo1, geo2, geo3, geo4 ,geo5,geo 6
    ! --- Geometrical variables constraints
    double precision exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up
    double precision exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2
    double precision geo2_low, geo4_up ,geo5_up, geo6_low   ! r_hc, r_ro, t_ro, l_v_ro, r_vh
    double precision geo2_low_2, geo3_low_2, geo4_low_2   ! Geometrical ! r_hc, l_com, e
    common/optimization_var_constraints/exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up, geo2_low, geo4_up ,geo5_up, geo6_low, exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2, geo2_low_2, geo3_low_2, geo4_low_2
    ! --- Geometrical array 
    !double precision, dimension (1:20) :: ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc
    double precision, dimension (:), allocatable :: ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc
    double precision, dimension (:), allocatable :: ori_r_oc_2, ori_r_ro_2, ori_r_shaft_2, ori_l_bearing_2, ori_dia_disc_2, ori_t_dv_2
    double precision, dimension (1:max_opt_reflect_no) :: re_r_oc, re_l_com, re_r_shaft, re_l_vh, re_w_vs_ro, re_w_v_ro, re_dia_suc, re_dia_disc
    double precision, dimension (1:max_opt_reflect_no) :: re_r_oc_2, re_r_ro_2, re_r_shaft_2, re_l_bearing_2, re_dia_disc_2, re_t_dv_2
    ! --- Performance variables
    double precision P_ind_hl_opt, P_input_motor_opt, P_ind_id_opt, P_input_opt, P_avrg_no_loss_opt, P_avrg_loss_opt, P_avrg_inertia_ro_opt, P_avrg_inertia_vh_opt, P_avrg_com_opt, P_valve_loss_opt, P_avrg_bear_s_opt, P_avrg_ecc_opt    ! power losses/input
    common/optimization_power/P_ind_hl_opt, P_input_motor_opt, P_ind_id_opt, P_input_opt, P_avrg_no_loss_opt, P_avrg_loss_opt, P_avrg_inertia_ro_opt, P_avrg_inertia_vh_opt, P_avrg_com_opt, P_valve_loss_opt, P_avrg_bear_s_opt, P_avrg_ecc_opt
    double precision T_peak_opt
    common/optimization_torque/T_peak_opt
    double precision P_loss_suc_hl_opt, P_loss_disc_hl_opt  ! Suction and discharge valve losses
    double precision L_avrg_ef_vh_opt, L_avrg_s_vh_opt, L_avrg_f_vs_opt, L_avrg_ef_ro_opt, L_avrg_lub_opt, L_avrg_reexpansion_opt, L_avrg_lip_seal_opt   ! Individual frictional losses
    common/optimization_loss/P_loss_suc_hl_opt, P_loss_disc_hl_opt, L_avrg_ef_vh_opt, L_avrg_s_vh_opt, L_avrg_f_vs_opt, L_avrg_ef_ro_opt, L_avrg_lub_opt, L_avrg_reexpansion_opt, L_avrg_lip_seal_opt
    ! --- Performance array
    !double precision, dimension (1:5) :: a_P_ind_hl_opt, a_P_ind_id_opt, a_P_input_opt, a_P_avrg_no_loss_opt, a_P_avrg_loss_opt, a_P_avrg_inertia_ro_opt, a_P_avrg_inertia_vh_opt, a_P_avrg_com_opt, a_P_valve_loss_opt, a_P_avrg_bear_s_opt
    !double precision, dimension (1:5) :: a_T_peak_opt, a_L_avrg_ef_vh_opt, a_L_avrg_s_vh_opt, a_L_avrg_f_vs_opt, a_L_avrg_ef_ro_opt, a_L_avrg_lub_opt  
    !double precision, dimension (1:5) :: a_obj_opt, a_eff_mec, a_COP_real
    double precision, dimension (:), allocatable :: a_P_ind_hl_opt, a_P_ind_id_opt, a_P_input_opt, a_P_avrg_no_loss_opt, a_P_avrg_loss_opt, a_P_avrg_inertia_ro_opt, a_P_avrg_inertia_vh_opt, a_P_avrg_com_opt, a_P_valve_loss_opt, a_P_avrg_bear_s_opt, a_P_avrg_ecc_opt
    double precision, dimension (:), allocatable :: a_T_peak_opt, a_L_avrg_ef_vh_opt, a_L_avrg_s_vh_opt, a_L_avrg_f_vs_opt, a_L_avrg_ef_ro_opt, a_L_avrg_lub_opt, a_L_avrg_reexpansion_opt  
    double precision, dimension (:), allocatable :: a_obj_opt, a_eff_2nd, a_eff_mec, a_COP_real, a_eff_vol
    double precision, dimension (1:max_opt_reflect_no) :: re_P_ind_hl_opt, re_P_ind_id_opt, re_P_input_opt, re_P_avrg_no_loss_opt, re_P_avrg_loss_opt, re_P_avrg_inertia_ro_opt, re_P_avrg_inertia_vh_opt, re_P_avrg_com_opt, re_P_valve_loss_opt, re_P_avrg_bear_s_opt, re_P_avrg_ecc_opt
    double precision, dimension (1:max_opt_reflect_no) :: re_T_peak_opt, re_L_avrg_ef_vh_opt, re_L_avrg_s_vh_opt, re_L_avrg_f_vs_opt, re_L_avrg_ef_ro_opt, re_L_avrg_lub_opt, re_L_avrg_reexpansion_opt 
    double precision, dimension (1:max_opt_reflect_no) :: re_obj_opt, re_eff_2nd, re_eff_mec, re_COP_real, re_eff_vol

    
    ! --- Dummy variables
    integer, dimension (1:1) :: best_loc, worst_loc
    double precision best_obj, worst_obj
    
    
    ! include "var_optimization.f90"