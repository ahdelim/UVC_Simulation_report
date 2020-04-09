! ----------------------------------------------------
! The optimization algorithm main body
! To use this optimization function
! 1.    Call and input the selected variables behind
! 2.    Edit the selected variables accordingly
! --------------------------------------

subroutine Optimization_main()
    implicit none
    include "var_simulation_parameter.f90"
    include "var_optimization.f90"
    include "var_main_dimensions.f90"
    include "var_power.f90"
    include "var_indicated_work.f90"
    include "var_compressor_evaluation.f90"
    integer worst_index_check, opt_idle_check_2   ! to check the number of consecutive iteration of the same worst index 
    double precision I_vh_O, T_lub      ! Moment of inertia about O of vane housing and rotor, lubricant frictional torque (between roller and rotor, assume constant first)
    double precision mass_vh, mass_rotor
    double precision T_avrg_input, T_avrg_no_loss, T_avrg_loss, T_avrg_I_ro, T_avrg_I_vh, T_avrg_com, T_avrg_bear_s, T_avrg_ecc, T_peak         ! average torque without any loss
    
    common/moment_of_inertia/mass_vh, mass_rotor, I_vh_O
    common/frictional_torque/T_lub
    common/dynamic_torque/T_avrg_input, T_avrg_no_loss, T_avrg_loss, T_avrg_I_ro, T_avrg_I_vh, T_avrg_com, T_avrg_bear_s, T_avrg_ecc, T_peak 
    ! --------------------------------
    ! Optimization parameters
    ! --------------------------------
    call optimization_parameters(opt_ori_complex, improv_check_cut_off, opt_cut_off, vol_min, vol_max, vol_max_2)

    ! --------------------------------
    ! Allocate array
    ! --------------------------------

    allocate(ori_r_oc(opt_ori_complex), ori_l_com(opt_ori_complex), ori_r_shaft(opt_ori_complex), ori_l_vh(opt_ori_complex), ori_w_vs_ro(opt_ori_complex), ori_w_v_ro(opt_ori_complex), ori_dia_suc(opt_ori_complex), ori_dia_disc(opt_ori_complex))
    allocate(ori_r_oc_2(opt_ori_complex), ori_r_ro_2(opt_ori_complex), ori_r_shaft_2(opt_ori_complex), ori_l_bearing_2(opt_ori_complex), ori_dia_disc_2(opt_ori_complex), ori_t_dv_2(opt_ori_complex))
    allocate(a_P_ind_hl_opt(opt_ori_complex), a_P_ind_id_opt(opt_ori_complex), a_P_input_opt(opt_ori_complex), a_P_avrg_no_loss_opt(opt_ori_complex), a_P_avrg_loss_opt(opt_ori_complex), a_P_avrg_inertia_ro_opt(opt_ori_complex), a_P_avrg_inertia_vh_opt(opt_ori_complex), a_P_avrg_com_opt(opt_ori_complex), a_P_valve_loss_opt(opt_ori_complex), a_P_avrg_bear_s_opt(opt_ori_complex),a_P_avrg_ecc_opt(opt_ori_complex))
    allocate(a_T_peak_opt(opt_ori_complex), a_L_avrg_ef_vh_opt(opt_ori_complex), a_L_avrg_s_vh_opt(opt_ori_complex), a_L_avrg_f_vs_opt(opt_ori_complex), a_L_avrg_ef_ro_opt(opt_ori_complex), a_L_avrg_lub_opt(opt_ori_complex),a_L_avrg_reexpansion_opt(opt_ori_complex))
    allocate(a_obj_opt(opt_ori_complex), a_eff_2nd(opt_ori_complex), a_eff_mec(opt_ori_complex), a_eff_vol(opt_ori_complex), a_COP_real(opt_ori_complex))

    ! --------------------------------
    ! Initialise optimization algorithm parameters
    ! --------------------------------
    opt_i = 1        ! Total number of iteration
    opt_conv_error = 1.0    ! Error of best and worst point 
    ! --------------------------------
    ! Assign first point to array
    ! Which is the point that the prototype used
    ! --------------------------------
    ori_r_oc(opt_i) = r_oc
    ori_l_com(opt_i) = l_com
    ori_r_shaft(opt_i) = r_shaft
    ori_l_vh(opt_i) = l_vh
    ori_w_vs_ro(opt_i) = w_vs_ro
    ori_w_v_ro(opt_i) = w_v_ro
    ori_dia_suc(opt_i) = dia_suc
    ori_dia_disc(opt_i) = dia_disc
    ! ----- Set 2 variables -----
    ori_r_oc_2(opt_i) = r_oc
    ori_r_ro_2(opt_i) = r_ro
    ori_r_shaft_2(opt_i) = r_shaft
    ori_l_bearing_2(opt_i) = l_bearing
    ori_dia_disc_2(opt_i) = dia_disc
    ori_t_dv_2(opt_i) = t_dv
    ! ---- Performance array
    a_P_ind_hl_opt(opt_i) = P_ind_hl
    a_P_ind_id_opt(opt_i) = P_ind_id
    a_P_input_opt(opt_i) = P_input
    a_P_avrg_no_loss_opt(opt_i) = P_avrg_no_loss 
    a_P_avrg_loss_opt(opt_i) = P_avrg_loss
    a_P_avrg_inertia_ro_opt(opt_i) = P_avrg_inertia_ro
    a_P_avrg_inertia_vh_opt(opt_i) = P_avrg_inertia_vh
    a_P_avrg_com_opt(opt_i) = P_avrg_com
    a_P_valve_loss_opt(opt_i) = P_valve_loss
    a_P_avrg_bear_s_opt(opt_i) = P_avrg_bear_s
    a_P_avrg_ecc_opt(opt_i) = P_avrg_ecc
    a_T_peak_opt(opt_i) = T_peak
    a_L_avrg_ef_vh_opt(opt_i) = L_avrg_ef_vh
    a_L_avrg_s_vh_opt(opt_i) = L_avrg_s_vh
    a_L_avrg_f_vs_opt(opt_i) = L_avrg_f_vs
    a_L_avrg_ef_ro_opt(opt_i) = L_avrg_ef_ro
    a_L_avrg_lub_opt(opt_i) = L_avrg_lub
    a_L_avrg_reexpansion_opt(opt_i) = L_avrg_reexpansion
    a_COP_real(opt_i) = COP_real
    a_eff_mec(opt_i) = eff_mec
    a_eff_2nd(opt_i) = eff_2nd
    a_eff_vol(opt_i) = eff_vol

    ! --------------------------------
    ! This block generates the variables randomly 
    ! Based on the number input "opt_ori_complex" (original complex)
    ! To use the optimization function
    ! --------------------------------
    do 1234 opt_i = 2, opt_ori_complex
            ! ----- Randomly generates the original complex
            !call optimization_random(r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc, r_hc, r_ro, t_ro, l_v_ro ,r_vh, e, ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc, ori_r_oc_2, ori_r_ro_2, ori_r_shaft_2, ori_l_bearing_2, ori_dia_disc_2, ori_t_dv_2)
            call optimization_random(ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc, ori_r_oc_2, ori_r_ro_2, ori_r_shaft_2, ori_l_bearing_2, ori_dia_disc_2, ori_t_dv_2)
            ! ----- mathematical model
            call optimization_thermo_main()
            ! ----- Compressor performance evaluation
            call optimization_performance()
            ! -------------------------------
            ! Assign points to original complex array
            ! -------------------------------
            a_P_ind_hl_opt(opt_i) = P_ind_hl_opt
            a_P_ind_id_opt(opt_i) = P_ind_id_opt
            a_P_input_opt(opt_i) = P_input_opt
            a_P_avrg_no_loss_opt(opt_i) = P_avrg_no_loss_opt 
            a_P_avrg_loss_opt(opt_i) = P_avrg_loss_opt
            a_P_avrg_inertia_ro_opt(opt_i) = P_avrg_inertia_ro_opt
            a_P_avrg_inertia_vh_opt(opt_i) = P_avrg_inertia_vh_opt
            a_P_avrg_com_opt(opt_i) = P_avrg_com_opt
            a_P_valve_loss_opt(opt_i) = P_valve_loss_opt
            a_P_avrg_bear_s_opt(opt_i) = P_avrg_bear_s_opt
            a_P_avrg_ecc_opt(opt_i) = P_avrg_ecc_opt
            a_T_peak_opt(opt_i) = T_peak_opt
            a_L_avrg_ef_vh_opt(opt_i) = L_avrg_ef_vh_opt
            a_L_avrg_s_vh_opt(opt_i) = L_avrg_s_vh_opt
            a_L_avrg_f_vs_opt(opt_i) = L_avrg_f_vs_opt
            a_L_avrg_ef_ro_opt(opt_i) = L_avrg_ef_ro_opt
            a_L_avrg_lub_opt(opt_i) = L_avrg_lub_opt
            a_L_avrg_reexpansion_opt(opt_i) = L_avrg_reexpansion_opt
            a_COP_real(opt_i) = COP_real
            a_eff_mec(opt_i) = eff_mec
            a_eff_2nd(opt_i) = eff_2nd
            a_eff_vol(opt_i) = eff_vol
            
    print *, ' --------------------------------------------------------------------------- '
    print'(1x,A,20F10.4)', " COP array: ", a_COP_real
    print *, ' ---------------------------------- '
    print'(1x,A,20F10.4)', " eff_mec array: ", a_eff_mec
    print *, ' ---------------------------------- '
    print'(1x,A,20F10.4)', " eff_2nd array: ", a_eff_2nd
    print *, ' ---------------------------------- '
    print'(1x,A,20F10.4)', " eff_vol array: ", a_eff_vol
    print *, ' --------------------------------------------------------------------------- '
1234 continue    
    ! -----------------------------------
    ! Initialization
    ! -----------------------------------
    worst_index_check = 0
    opt_idle_check = 1
    opt_idle_check_2 = 1
    opt_re = 1
    ! ---------------------------------
    ! Allocate first point to reflection array
    ! ---------------------------------
    re_r_oc(opt_re) = ori_r_oc(1)
    re_l_com(opt_re) = ori_l_com(1)
    re_r_shaft(opt_re) = ori_r_shaft(1)
    re_l_vh(opt_re) = ori_l_vh(1)
    re_w_vs_ro(opt_re) = ori_w_vs_ro(1)
    re_w_v_ro(opt_re) = ori_w_v_ro(1)
    re_dia_suc(opt_re) = ori_dia_suc(1)
    re_dia_disc(opt_re) = ori_dia_disc(1)
    ! ----- Set 2 variables -----
    re_r_oc_2(opt_re) = ori_r_oc_2(1)
    re_r_ro_2(opt_re) = ori_r_ro_2(1)
    re_r_shaft_2(opt_re) = ori_r_shaft_2(1)
    re_l_bearing_2(opt_re) = ori_l_bearing_2(1)
    re_dia_disc_2(opt_re) = ori_dia_disc_2(1)
    re_t_dv_2(opt_re) = ori_t_dv_2(1)
    ! ---- Performance array
    re_P_ind_hl_opt(opt_re) = a_P_ind_hl_opt(1)
    re_P_ind_id_opt(opt_re) = a_P_ind_id_opt(1)
    re_P_input_opt(opt_re) = a_P_input_opt(1)
    re_P_avrg_no_loss_opt(opt_re) = a_P_avrg_no_loss_opt(1)
    re_P_avrg_loss_opt(opt_re) = a_P_avrg_loss_opt(1)
    re_P_avrg_inertia_ro_opt(opt_re) = a_P_avrg_inertia_ro_opt(1)
    re_P_avrg_inertia_vh_opt(opt_re) = a_P_avrg_inertia_vh_opt(1)
    re_P_avrg_com_opt(opt_re) = a_P_avrg_com_opt(1)
    re_P_valve_loss_opt(opt_re) = a_P_valve_loss_opt(1)
    re_P_avrg_bear_s_opt(opt_re) = a_P_avrg_bear_s_opt(1)
    re_P_avrg_ecc_opt(opt_re) = a_P_avrg_ecc_opt(1)
    re_T_peak_opt(opt_re) = a_T_peak_opt(1)
    re_L_avrg_ef_vh_opt(opt_re) = a_L_avrg_ef_vh_opt(1)
    re_L_avrg_s_vh_opt(opt_re) = a_L_avrg_s_vh_opt(1)
    re_L_avrg_f_vs_opt(opt_re) = a_L_avrg_f_vs_opt(1)
    re_L_avrg_ef_ro_opt(opt_re) = a_L_avrg_ef_ro_opt(1)
    re_L_avrg_lub_opt(opt_re) = a_L_avrg_lub_opt(1)
    re_L_avrg_reexpansion_opt(opt_re) = a_L_avrg_reexpansion_opt(1)
    re_COP_real(opt_re) = a_COP_real(1)
    re_eff_mec(opt_re) = a_eff_mec(1)
    re_eff_2nd(opt_re) = a_eff_2nd(1)
    re_eff_vol(opt_re) = a_eff_vol(1)
    ! -------------------------------
    ! Assign objective function  
    ! -------------------------------
    if (obj_func_input .eq. 1) then
        a_obj_opt = a_COP_real
        re_obj_opt = re_COP_real
    elseif (obj_func_input .eq. 2) then
        a_obj_opt = a_eff_mec
        re_obj_opt = re_eff_mec
    elseif (obj_func_input .eq. 3) then
        a_obj_opt = a_eff_2nd
        re_obj_opt = re_eff_2nd
    elseif (obj_func_input .eq. 4) then
        a_obj_opt = a_eff_vol
        re_obj_opt = re_eff_vol
    endif
    ! ---------------------------------
    ! Checking convergence error
    ! and 
    ! Assigning best and worst points
    ! ---------------------------------
    call optimization_conv_check(opt_conv_error, a_obj_opt, opt_ori_complex, best_obj, best_loc, worst_obj, worst_loc)
    print *, ' --------------------------------------------------------------------------- '
    print'(1x,5F10.4)', a_obj_opt
    print('(1x,A,F10.4,A,I10)'), "The maximum value in the original complex = ",best_obj, " Index = ", best_loc(1)
    print('(1x,A,F10.4,A,I10)'), "The minimum value in the original complex = ",worst_obj, " Index = ", worst_loc(1)
    print *, ' --------------------------------------------------------------------------- '
    ! -----------------------------------
    ! Create headers for file
    ! Write the first point
    ! -----------------------------------
    if (obj_var_set == 1) then
            write(152,2981), "Iteration","Obj.Function","eff_2nd","eff_mec","eff_vol","COP_real","r_oc", "l_com", "r_shaft", "l_vh", "w_vs_ro", "w_v_ro", "dia_suc", "dia_disc"
        else
            write(152,2981), "Iteration","Obj.Function","eff_2nd","eff_mec","eff_vol","COP_real","r_oc", "r_ro", "r_shaft", "l_bearing", "dia_disc", "t_dv"
        endif
    !write(152,2981), "Iteration","Obj.Function","eff_2nd","eff_mec","COP_real","r_oc", "l_com", "r_shaft", "l_vh", "w_vs_ro", "w_v_ro", "dia_suc", "dia_disc"
    write(153,2981), "Iteration", "re_P_ind_hl_opt", "re_P_ind_id_opt", "re_P_input_opt", "re_P_avrg_no_loss_opt", "re_P_avrg_loss_opt", "re_P_avrg_inertia_ro_opt", "re_P_avrg_inertia_vh_opt", "re_P_avrg_com_opt", "re_P_valve_loss_opt", "re_P_avrg_bear_s_opt"
    write(154,2981), "Iteration", "re_T_peak_opt", "re_L_avrg_ef_vh_opt", "re_L_avrg_s_vh_opt", "re_L_avrg_f_vs_opt", "re_L_avrg_ef_ro_opt", "re_L_avrg_lub_opt"
    if (obj_var_set == 1) then
            write(152,2982), opt_re, re_obj_opt(opt_re), re_eff_2nd(opt_re), re_eff_mec(opt_re), re_eff_vol(opt_re), re_COP_real(opt_re), re_r_oc(opt_re), re_l_com(opt_re), re_r_shaft(opt_re), re_l_vh(opt_re), re_w_vs_ro(opt_re), re_w_v_ro(opt_re), re_dia_suc(opt_re), re_dia_disc(opt_re)
    else
            write(152,2982), opt_re, re_obj_opt(opt_re), re_eff_2nd(opt_re), re_eff_mec(opt_re), re_eff_vol(opt_re), re_COP_real(opt_re), re_r_oc_2(opt_re), re_r_ro_2(opt_re), re_r_shaft_2(opt_re), re_l_bearing_2(opt_re), re_dia_disc_2(opt_re), re_t_dv_2(opt_re)
    endif
    write(153,2982), opt_re, re_P_ind_hl_opt(opt_re), re_P_ind_id_opt(opt_re), re_P_input_opt(opt_re), re_P_avrg_no_loss_opt(opt_re), re_P_avrg_loss_opt(opt_re), re_P_avrg_inertia_ro_opt(opt_re), re_P_avrg_inertia_vh_opt(opt_re), re_P_avrg_com_opt(opt_re), re_P_valve_loss_opt(opt_re), re_P_avrg_bear_s_opt(opt_re)
    write(154,2982), opt_re, re_T_peak_opt(opt_re), re_L_avrg_ef_vh_opt(opt_re), re_L_avrg_s_vh_opt(opt_re), re_L_avrg_f_vs_opt(opt_re), re_L_avrg_ef_ro_opt(opt_re), re_L_avrg_lub_opt(opt_re)
    ! ----------------------------
    ! Start reflection
    ! ----------------------------
    do while (opt_conv_error > opt_conv_cri)
        ! ----- Reflected design parameters
        call optimization_reflection(ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc, ori_r_oc_2, ori_r_ro_2, ori_r_shaft_2, ori_l_bearing_2, ori_dia_disc_2, ori_t_dv_2, best_loc, worst_loc, best_obj, worst_obj, a_obj_opt)
189 continue    ! After idle check, worst complex will move halfway toward best complex, and continue from here, check line 334        
        ! ----- mathematical model
        call optimization_thermo_main()
        ! ----- Compressor performance evaluation
        call optimization_performance()  
        ! --- increment
        opt_re = opt_re + 1
        ! -------------------------------
        ! Assign reflected to original complex array
        ! -------------------------------
        a_P_ind_hl_opt(worst_loc(1)) = P_ind_hl_opt
        a_P_ind_id_opt(worst_loc(1)) = P_ind_id_opt
        a_P_input_opt(worst_loc(1)) = P_input_opt
        a_P_avrg_no_loss_opt(worst_loc(1)) = P_avrg_no_loss_opt 
        a_P_avrg_loss_opt(worst_loc(1)) = P_avrg_loss_opt
        a_P_avrg_inertia_ro_opt(worst_loc(1)) = P_avrg_inertia_ro_opt
        a_P_avrg_inertia_vh_opt(worst_loc(1)) = P_avrg_inertia_vh_opt
        a_P_avrg_com_opt(worst_loc(1)) = P_avrg_com_opt
        a_P_valve_loss_opt(worst_loc(1)) = P_valve_loss_opt
        a_P_avrg_bear_s_opt(worst_loc(1)) = P_avrg_bear_s_opt
        a_P_avrg_ecc_opt(worst_loc(1)) = P_avrg_ecc_opt
        a_T_peak_opt(worst_loc(1)) = T_peak_opt
        a_L_avrg_ef_vh_opt(worst_loc(1)) = L_avrg_ef_vh_opt
        a_L_avrg_s_vh_opt(worst_loc(1)) = L_avrg_s_vh_opt
        a_L_avrg_f_vs_opt(worst_loc(1)) = L_avrg_f_vs_opt
        a_L_avrg_ef_ro_opt(worst_loc(1)) = L_avrg_ef_ro_opt
        a_L_avrg_lub_opt(worst_loc(1)) = L_avrg_lub_opt
        a_L_avrg_reexpansion_opt(worst_loc(1)) = L_avrg_reexpansion_opt
        a_COP_real(worst_loc(1)) = COP_real
        a_eff_mec(worst_loc(1)) = eff_mec
        a_eff_2nd(worst_loc(1)) = eff_2nd
        a_eff_vol(worst_loc(1)) = eff_vol
        ! ------------------------------
        ! Assign reflected point to array
        ! ------------------------------
        re_r_oc(opt_re) = r_oc
        re_l_com(opt_re) = l_com
        re_r_shaft(opt_re) = r_shaft
        re_l_vh(opt_re) = l_vh
        re_w_vs_ro(opt_re) = w_vs_ro
        re_w_v_ro(opt_re) = w_v_ro
        re_dia_suc(opt_re) = dia_suc
        re_dia_disc(opt_re) = dia_disc
        
        ! ----- Set 2 variables -----
        re_r_oc_2(opt_re) = r_oc
        re_r_ro_2(opt_re) = r_ro
        re_r_shaft_2(opt_re) = r_shaft
        re_l_bearing_2(opt_re) = l_bearing
        re_dia_disc_2(opt_re) = dia_disc
        re_t_dv_2(opt_re) = t_dv
        ! ---- Performance array
        re_P_ind_hl_opt(opt_re) = P_ind_hl_opt
        re_P_ind_id_opt(opt_re) = P_ind_id_opt
        re_P_input_opt(opt_re) = P_input_opt
        re_P_avrg_no_loss_opt(opt_re) = P_avrg_no_loss_opt 
        re_P_avrg_loss_opt(opt_re) = P_avrg_loss_opt
        re_P_avrg_inertia_ro_opt(opt_re) = P_avrg_inertia_ro_opt
        re_P_avrg_inertia_vh_opt(opt_re) = P_avrg_inertia_vh_opt
        re_P_avrg_com_opt(opt_re) = P_avrg_com_opt
        re_P_valve_loss_opt(opt_re) = P_valve_loss_opt
        re_P_avrg_bear_s_opt(opt_re) = P_avrg_bear_s_opt
        a_P_avrg_ecc_opt(opt_re) = P_avrg_ecc_opt
        re_T_peak_opt(opt_re) = T_peak_opt
        re_L_avrg_ef_vh_opt(opt_re) = L_avrg_ef_vh_opt
        re_L_avrg_s_vh_opt(opt_re) = L_avrg_s_vh_opt
        re_L_avrg_f_vs_opt(opt_re) = L_avrg_f_vs_opt
        re_L_avrg_ef_ro_opt(opt_re) = L_avrg_ef_ro_opt
        re_L_avrg_lub_opt(opt_re) = L_avrg_lub_opt
        re_L_avrg_reexpansion_opt(opt_re) = L_avrg_reexpansion_opt
        re_COP_real(opt_re) = COP_real
        re_eff_mec(opt_re) = eff_mec
        re_eff_2nd(opt_re) = eff_2nd
        re_eff_vol(opt_re) = eff_vol
        ! -------------------------------
        ! Assign objective function  
        ! -------------------------------
        if (obj_func_input .eq. 1) then
            a_obj_opt = a_COP_real
            re_obj_opt = re_COP_real
        elseif (obj_func_input .eq. 2) then
            a_obj_opt = a_eff_mec
            re_obj_opt = re_eff_mec
        elseif (obj_func_input .eq. 3) then
            a_obj_opt = a_eff_2nd
            re_obj_opt = re_eff_2nd
        elseif (obj_func_input .eq. 4) then
            a_obj_opt = a_eff_vol
            re_obj_opt = re_eff_vol
        endif
        ! ----- Error convergence checking ----- !
        call optimization_conv_check(opt_conv_error, a_obj_opt, opt_ori_complex, best_obj, best_loc, worst_obj, worst_loc)  
        print*, ' ******************************************************** '
        print'(1x,A,I10)', ' Iteration No. : ' , opt_re
        print'(1x,A,F10.4,A)', ' % Difference between best and worst points     = ', opt_conv_error*100.0, ' %'
        print'(1x,A,F10.4)',   ' Real Difference between best and worst points  = ', best_obj - worst_obj
        print('(1x,A,F10.4,A,I10)'), "The best = ",best_obj, " Index = ", best_loc(1)
        print('(1x,A,F10.4,A,I10)'), "The worst = ",worst_obj, " Index = ", worst_loc(1)
        print'(1x,5F10.4)', a_obj_opt
        print*, ' -------------------------------------------------------- '
        ! ---------------- !
        ! to check error 
        ! ---------------- !
        if (opt_conv_error > 0.95) then
            print'(1x,5F10.4)', a_obj_opt
            print'(1x,5F10.4)', a_COP_real
            print'(1x,5F10.4)', a_eff_mec
            print'(1x,5F10.4)', a_eff_2nd
            print'(1x,5F10.4)', a_eff_vol
            goto 188
        endif
        if (obj_var_set == 1) then
            write(152,2982), opt_re, re_obj_opt(opt_re), re_eff_2nd(opt_re), re_eff_mec(opt_re), re_eff_vol(opt_re), re_COP_real(opt_re), re_r_oc(opt_re), re_l_com(opt_re), re_r_shaft(opt_re), re_l_vh(opt_re), re_w_vs_ro(opt_re), re_w_v_ro(opt_re), re_dia_suc(opt_re), re_dia_disc(opt_re)
        else
            write(152,2982), opt_re, re_obj_opt(opt_re), re_eff_2nd(opt_re), re_eff_mec(opt_re), re_eff_vol(opt_re), re_COP_real(opt_re), re_r_oc_2(opt_re), re_r_ro_2(opt_re), re_r_shaft_2(opt_re), re_l_bearing_2(opt_re), re_dia_disc_2(opt_re), re_t_dv_2(opt_re)
        endif
        write(153,2982), opt_re, re_P_ind_hl_opt(opt_re), re_P_ind_id_opt(opt_re), re_P_input_opt(opt_re), re_P_avrg_no_loss_opt(opt_re), re_P_avrg_loss_opt(opt_re), re_P_avrg_inertia_ro_opt(opt_re), re_P_avrg_inertia_vh_opt(opt_re), re_P_avrg_com_opt(opt_re), re_P_valve_loss_opt(opt_re), re_P_avrg_bear_s_opt(opt_re)
        write(154,2982), opt_re, re_T_peak_opt(opt_re), re_L_avrg_ef_vh_opt(opt_re), re_L_avrg_s_vh_opt(opt_re), re_L_avrg_f_vs_opt(opt_re), re_L_avrg_ef_ro_opt(opt_re), re_L_avrg_lub_opt(opt_re)
        ! ---------------------------------------------
        ! Iteration checking
        ! if the worst_index is always the same index for consecutive 5 iteration
        ! the point will move toward best complex for by half
        ! ---------------------------------------------
        
        if (worst_loc(1) == worst_index_check) then  
            opt_idle_check = opt_idle_check + 1
            print'(1x,A,5I)',' Same worst index detected, consecutive iteration: ',opt_idle_check
            if (opt_idle_check > improv_check_cut_off) then
                print'(1x,A)',' 5 (or more) consecutive iterations detected '
                print'(1x,A)',' Applying improvement check, moving worst point toward best point by halfway...'   
                opt_idle_check_2 = opt_idle_check_2 + 1
                if (opt_idle_check_2 > improv_check_cut_off) then
                    print'(1x,A)',' This index idle for too long '
                    print'(1x,A)',' Moving worst point toward best point...'
                    call optimization_force_move(ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc, ori_r_oc_2, ori_r_ro_2, ori_r_shaft_2, ori_l_bearing_2, ori_dia_disc_2, ori_t_dv_2, best_loc, worst_loc, best_obj, worst_obj, a_obj_opt)
                    opt_idle_check = 1
                    opt_idle_check_2 = 1
                    goto 189
                endif
                call optimization_improv_move(ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc, ori_r_oc_2, ori_r_ro_2, ori_r_shaft_2, ori_l_bearing_2, ori_dia_disc_2, ori_t_dv_2, best_loc, worst_loc, best_obj, worst_obj, a_obj_opt)
                !opt_idle_check = 1
                goto 189
            endif
        else
            print'(1x,A)',' Different worst index number, reset consecutive iteration number to 1'
            worst_index_check = worst_loc(1)
            opt_idle_check = 1
            opt_idle_check_2 = 1
        endif
        
        if (opt_re > opt_cut_off) then
            goto 188
        endif
    enddo
188 continue        
    ! ---------------------------------------------
    ! Show the best performed point
    ! ---------------------------------------------
    print*, ' '
    print*, ' '
    print*, ' '
    print*, ' ======================================================== '
    print'(1x,A)', ' Optimization done, evaluating best complex... '
    PRINT*, ' -------------------------------------------------------- '
    call optimization_conv_check(opt_conv_error, a_obj_opt, opt_ori_complex, best_obj, best_loc, worst_obj, worst_loc)  
    print*, ' ******************************************************** '
    print'(1x,A,I10)', ' Iteration No. : ' , opt_re
    print'(1x,A,F10.4,A)', ' % Difference between best and worst points     = ', opt_conv_error*100.0, ' %'
    print'(1x,A,F10.4)',   ' Real Difference between best and worst points  = ', best_obj - worst_obj
    print('(1x,A,F10.4,A,I10)'), "The best = ",best_obj, " Index = ", best_loc(1)
    print('(1x,A,F10.4,A,I10)'), "The worst = ",worst_obj, " Index = ", worst_loc(1)
    print'(1x,5F10.4)', a_obj_opt
    print*, ' -------------------------------------------------------- '
    call optimization_best_geo(ori_r_oc, ori_l_com, ori_r_shaft, ori_l_vh, ori_w_vs_ro, ori_w_v_ro, ori_dia_suc, ori_dia_disc, ori_r_oc_2, ori_r_ro_2, ori_r_shaft_2, ori_l_bearing_2, ori_dia_disc_2, ori_t_dv_2, best_loc)
    call optimization_thermo_main()
    call optimization_performance()
    call optimization_best()
    print*, ' ------------------ END OF OPTIMIZATION ------------------ '
    
2988 format (1x,A,F15.4,A)
2981 format (16A25)
2982 format (I25, 14ES25.6) 
endsubroutine optimization_main