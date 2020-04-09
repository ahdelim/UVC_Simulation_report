! -----------------------------------------------------------
! Subroutine to prompt the selection of objective function for optimization
! -----------------------------------------------------------
    
subroutine optimization_obj_select(obj_func_input, obj_var_set)
    implicit none
    integer obj_func_input, obj_var_set 
    integer check_int
    print *, ' ---> Please select objective function (Input number)' ! Selection of objective function based on 
    print *, ' '
    print *, ' 1 - COP'
    print *, ' 2 - Mechanical Efficiency'
    print *, ' 3 - Second Law Efficiency'
    print *, ' 4 - Volumetric Efficiency'
    print *, ' '
1542 continue    
    read (*,*,iostat=check_int), obj_func_input
    write (151,*) " ==================================================================== "
    write (151,*) " Settings of Optimization "
    write (151,*) " -------------------------------------------------------------------- "
    if (check_int == 0) then
        if (obj_func_input .eq. 1) then
            print *, ' ---> Selected Objective Function = COP'
            write (151,*), ' Objective Function     = COP'
        elseif (obj_func_input .eq. 2) then
            print *, ' ---> Selected Objective Function = Mechanical Efficiency'
            write (151,*), ' Objective Function     = Mechanical Efficiency'
        elseif (obj_func_input .eq. 3) then
            print *, ' ---> Selected Objective Function = Second Law Efficiency'
            write (151,*), ' Objective Function     = Second Law Efficiency'
        elseif (obj_func_input .eq. 4) then
            print *, ' ---> Selected Objective Function = Volumetric Efficiency'
            write (151,*), ' Objective Function     = Volumetric Efficiency'
        else
            print *, ' Please input again '
            goto 1542
        endif
    endif
    
    print *, ' ---> Please select set of variables to be optimized (Input number)' ! Selection of variables to be optimized
    print *, ' '
    print *, ' 1 - r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc'
    print *, ' 2 - r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv'
    print *, ' '
1543 continue    
    read (*,*,iostat=check_int), obj_var_set
    if (check_int == 0) then
        if (obj_var_set .eq. 1) then
            print *, ' ---> Selected set of variables = r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc'
            write (151,*), ' Design variables = r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc'
        elseif (obj_var_set .eq. 2) then
            print *, ' ---> Selected set of variables = r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv'
            write (151,*), ' Design variables = r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv'
        else
            print *, ' Please input again '
            goto 1543
        endif
    endif
    write (151,*) " -------------------------------------------------------------------- "

endsubroutine optimization_obj_select
    
    

! -----------------------------------------------------------
! Subroutine to calculate optimization parameters
! -----------------------------------------------------------
subroutine optimization_parameters(opt_ori_complex, improv_check_cut_off, opt_cut_off, vol_min, vol_max, vol_max_2)   
    implicit none
    ! ----- optimization parameters ----- !
    double precision factor_a, factor_b
    common/optimization_para_re/factor_a, factor_b
    integer opt_ori_complex, improv_check_cut_off, opt_cut_off      ! Total number of the original complex pool, check for improvement cut off number, and iteration cut off number
    double precision vol_min, vol_max, vol_max_2       ! minimum and maximum compressor volume/capacity 
    ! ----- Constraints of variables
    ! --- Geometrical variables constraints
    double precision exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up
    double precision exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2
    double precision geo2_low, geo4_up ,geo5_up, geo6_low   ! r_hc, r_ro, t_ro, l_v_ro, r_vh
    double precision geo2_low_2, geo3_low_2, geo4_low_2 ! Geometrical ! l_com, e
    common/optimization_var_constraints/exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up, geo2_low, geo4_up ,geo5_up, geo6_low, exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2, geo2_low_2, geo3_low_2, geo4_low_2
    
    read(117,*) opt_ori_complex  ! Total number of results in random pool
    read(117,*) improv_check_cut_off    ! Number of iteration to cut off, for improvement checking
    read(117,*) opt_cut_off ! Total number of iteration to cut off if no convergence is found
    read(117,*) factor_a
    read(117,*) factor_b
    ! -----------------------------------
    ! Set 1 design variables: r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
    ! Total number of design variables: 8
    ! Compressor volumetric capacity is not fixed
    !------------------------------------
    read(117,*) vol_min     ! [cc]
    read(117,*) vol_max     ! [cc]
    read(117,*) vol_max_2   ! [cc]
    vol_min = vol_min*1.0d3 ! convert [cc] to [mm3]
    vol_max = vol_max*1.0d3 ! convert [cc] to [mm3]
    vol_max_2 = vol_max_2*1.0d3 ! convert [cc] to [mm3]
        
    read(117,*)exp1_low ! r_oc (set 1)
    read(117,*)exp1_up
    read(117,*)exp2_low ! l_com (set 1)
    read(117,*)exp2_up
    read(117,*)exp3_low ! r_shaft (set 1)
    read(117,*)exp3_up  
    read(117,*)exp4_low ! l_vh (set 1)
    read(117,*)exp4_up
    read(117,*)exp5_low ! w_vs_ro (set 1)
    read(117,*)exp5_up
    read(117,*)exp6_low ! w_v_ro (set 1)
    read(117,*)exp6_up
    read(117,*)exp7_low ! dia_suc (set 1)
    read(117,*)exp7_up 
    read(117,*)exp8_low ! dia_disc (set 1)
    read(117,*)exp8_up
    
    read(117,*)geo2_low ! r_ro lower
    read(117,*)geo4_up ! l_v_ro upper
    read(117,*)geo5_up ! r_vh upper
    read(117,*)geo6_low ! e lower
    
    ! -----------------------------------
    ! Set 2 design variables: r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv
    ! Total number of design variables: 6
    ! Compressor volumetric capacity is fixed
    !------------------------------------
    
    read(117,*)exp1_low_2 ! r_oc (set 2)
    read(117,*)exp1_up_2
    read(117,*)exp2_low_2 ! r_ro (set 2)
    read(117,*)exp2_up_2
    read(117,*)exp3_low_2 ! r_shaft (set 2)
    read(117,*)exp3_up_2  
    read(117,*)exp4_low_2 ! l_bearing (set 2)
    read(117,*)exp4_up_2
    read(117,*)exp5_low_2 ! dia_disc (set 2)
    read(117,*)exp5_up_2
    read(117,*)exp6_low_2 ! t_dv (set 2)
    read(117,*)exp6_up_2
    
    read(117,*)geo2_low_2   ! l_com
    read(117,*)geo3_low_2   ! e
    read(117,*)geo4_low_2  ! t_ro

    
    write (151,*) " -------------------------------------------------------------------- "
    write (151,*) " Fixed Conditions for Optimization "
    write (151,*) " -------------------------------------------------------------------- "
    write (151,2989) " Maximum compressor volume        vol_min         = ", vol_min/1.0d3, " cc"
    write (151,2989) " Maximum compressor volume        vol_max         = ", vol_max/1.0d3, " cc"
    write (151,*) " -------------------------------------------------------------------- "
    write (151,*) " Constraints of Set 1 Variables "
    write (151,*) " -------------------------------------------------------------------- "
    write (151,2989) " Lower constraints of r_oc        exp1_low        = ", exp1_low, " mm"
    write (151,2989) " Upper constraints of r_oc        exp1_up         = ", exp1_up, " mm"
    write (151,2989) " Lower constraints of l_com       exp2_low        = ", exp2_low, " mm"
    write (151,2989) " Upper constraints of l_com       exp2_up         = ", exp2_up, " mm"
    write (151,2989) " Lower constraints of r_shaft     exp3_low        = ", exp3_low, " mm"
    write (151,2989) " Upper constraints of r_shaft     exp3_up         = ", exp3_up, " mm"
    write (151,2989) " Lower constraints of l_vh        exp4_low        = ", exp4_low, " mm"
    write (151,2989) " Upper constraints of l_vh        exp4_up         = ", exp4_up, " mm"
    write (151,2989) " Lower constraints of w_vs_ro     exp5_low        = ", exp5_low, " mm"
    write (151,2989) " Upper constraints of w_vs_ro     exp5_up         = ", exp5_up, " mm"
    write (151,2989) " Lower constraints of w_v_ro      exp6_low        = ", exp6_low, " mm"
    write (151,2989) " Upper constraints of w_v_ro      exp6_up         = ", exp6_up, " mm"
    write (151,2989) " Lower constraints of dia_suc     exp7_low        = ", exp7_low, " mm"
    write (151,2989) " Upper constraints of dia_suc     exp7_up         = ", exp7_up, " mm"
    write (151,2989) " Lower constraints of dia_disc    exp8_low        = ", exp8_low, " mm"
    write (151,2989) " Upper constraints of dia_disc    exp8_up         = ", exp8_up, " mm"
    write (151,*) " -------------------------------------------------------------------- "
    write (151,*) " Constraints of Set 2 Variables "
    write (151,*) " -------------------------------------------------------------------- "
    write (151,2989) " Lower constraints of r_oc        exp1_low_2      = ", exp1_low_2, " mm"
    write (151,2989) " Upper constraints of r_oc        exp1_up_2       = ", exp1_up_2, " mm"
    write (151,2989) " Lower constraints of r_ro        exp2_low_2      = ", exp2_low_2, " mm"
    write (151,2989) " Upper constraints of r_ro        exp2_up_2       = ", exp2_up_2, " mm"
    write (151,2989) " Lower constraints of r_shaft     exp3_low_2      = ", exp3_low_2, " mm"
    write (151,2989) " Upper constraints of r_shaft     exp3_up_2       = ", exp3_up_2, " mm"
    write (151,2989) " Lower constraints of l_bearing   exp4_low_2      = ", exp4_low_2, " mm"
    write (151,2989) " Upper constraints of l_bearing   exp4_up_2       = ", exp4_up_2, " mm"
    write (151,2989) " Lower constraints of dia_disc    exp5_low_2      = ", exp5_low_2, " mm"
    write (151,2989) " Upper constraints of dia_disc    exp5_up_2       = ", exp5_up_2, " mm"
    write (151,2989) " Lower constraints of t_dv        exp6_low_2      = ", exp6_low_2, " mm"
    write (151,2989) " Upper constraints of t_dv        exp6_up_2       = ", exp6_up_2, " mm"
    write (151,*) " -------------------------------------------------------------------- "
    write (151,*) " Reflection parameters "
    write (151,*) " -------------------------------------------------------------------- "
    write (151,2989) " Reflection factor                factor_a        = ", factor_a, " -"
    write (151,2989) " Boundary factor                  factor_b        = ", factor_b, " -"
    
2989 format (1x,A,F15.2,A)
endsubroutine optimization_parameters
    
! ----------------------------------------------
! Subroutine for convergence, error and best/worst point assigning
! ----------------------------------------------
subroutine optimization_conv_check(opt_conv_error, array_obj_func, array_size, best, best_index, worst, worst_index)
    integer array_size
    double precision opt_conv_error, best, worst
    double precision, dimension (1:array_size) :: array_obj_func
    integer, dimension (1:1) :: best_index, worst_index
    best_index = maxloc(array_obj_func)
    worst_index = minloc(array_obj_func)
    !best = maxval(array_obj_func)
    !worst = minval(array_obj_func)
    best = array_obj_func(best_index(1))
    worst = array_obj_func(worst_index(1))
    opt_conv_error = (best - worst)/best
endsubroutine
    
    
! ----------------------------------------------
! Subroutine for explicit constraints check
! ----------------------------------------------
subroutine optimization_exp_constraint(exp, exp_up, exp_low, factor_b)
    double precision exp,exp_up,exp_low,factor_b
    
    if (exp > exp_up) then
        exp = exp_up - factor_b
        print'(1x,A,F10.4,A,F10.4)', ' Explicit upper constraints, ',exp_up,' violated, new = ',exp
    elseif (exp < exp_low) then
        exp = exp_low + factor_b
        print'(1x,A,F10.4,A,F10.4)', ' Explicit lower constraints, ',exp_low,' violated, new = ',exp
    endif
    
endsubroutine
    
    
    
! -------------------------------------
! Subroutine to write the result of best complex into files
! -------------------------------------
    
subroutine optimization_best
    implicit none
    include 'var_operational_parameter.f90'
    include 'var_simulation_parameter.f90'
    include "var_geometrical.f90"
    include "var_geometrical_diff.f90"
    include "var_thermo.f90"
    include "var_dynamic.f90"
    include "var_indicated_work.f90"
    include "var_compressor_evaluation.f90"
    include "var_operating_fluid_condition.f90"
    include "var_power.f90"
    include "var_leakage.f90"
    include "var_optimization.f90"
    include "var_exergy.f90"
    
    ! -------------------------------------------
    ! Mass flow rate
    ! -------------------------------------------
    mass_total_output = mass_total - avrg_leakage_mass  ! mass_total is obtained in thermo_with_heat_leakage.f90, avrg_leakage_mass is obtained in leakage_model.f90
    
    ! -------------------------------------------
    ! Cooling Capacity
    ! -------------------------------------------
    q_capacity = mass_total_output*(h_suc - h_bef_evap)       ! Cooling capacity in [W]
    
    
    ! -------------------------------------------
    ! Coefficient of Performance (COP)
    ! -------------------------------------------
    !COP_ideal = q_capacity/P_ind_id             ! Ideal COP with ideal indicated power
    !COP_w_o_hl = q_capacity/P_ind             ! COP without frictional power losses and without heat and leakage 
    COP_w_hl = q_capacity/P_ind_hl_opt              ! COP with heat and leakage but no frictional loss
    COP_real = q_capacity/P_input_opt               ! Real COP
    
    ! ------------------------------------------
    ! Power input 
    ! ------------------------------------------
    P_input_motor_opt = P_input_opt/eff_motor   ! Avrg power input from motor [W]
    
    ! -------------------------------------------
    ! Efficiencies Evaluation
    ! Volumetric, Mechanical, Isentropic
    ! -------------------------------------------
    eff_comp = P_ind_id_opt/(P_ind_id_opt + P_valve_loss_opt)
    eff_vol = (mass_total - avrg_leakage_mass)/m_flow_total_id        ! Actual_mass/theorectical_mass
    eff_vol_2 = (mass_total - avrg_leakage_mass)/mass_suct_in       ! mass discharged/mass entered
    !eff_mec = P_ind_hl/(P_ind_hl + P_avrg_loss)     ! Mechanical efficiency = Compression P/(Compression + mechanical losses) (this one by using PV diagram area)
    !eff_mec = P_avrg_com_opt/(P_avrg_com_opt + P_avrg_loss_opt)
    !eff_overall = eff_comp*eff_vol*eff_mec*eff_motor
    eff_mec = P_avrg_no_loss_opt/P_input_opt     ! total power used without losses over total power input           !P_avrg_com/(P_avrg_com + P_avrg_loss) ! + P_avrg_thr_loss)
    eff_overall = P_avrg_no_loss_opt/P_input_motor_opt  ! Overall efficiency, compression power/all losses term     !eff_comp*eff_vol*eff_mec*eff_motor
    
    write (155,*), " ============================================================================ "
    write (155,*), " Actual Compressor Performance "
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,2990) "Total Volume                          vol_total           = ", vol_total/1000., " cc or cm3"
    write (155,2990) "Residual mass (Dead volume)           mass_residual_dead  = ", mass_residual_dead*1000, " g"
    write (155,2990) "Avrg. Leakage Mass Flow               avrg_leakage_mass   = ", avrg_leakage_mass*1000, " g/s"
    write (155,2990) "Total Actual Mass Flow Rate           mass_total_output   = ", mass_total_output, " kg/s"
    write (155,2990) "Cubic Feet per Minute (CFM)                               = ", mass_total_output/rho_disc*35.3147*60, " CFM"
    write (155,2990) "Cooling Capacity                        q_capacity        = ", q_capacity, " W"
    write (155,2990) "Indicated Work (PV)                     P_ind_hl          = ", P_ind_hl_opt, " W"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,*), " Coefficienct of Performance (COP)"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,2990) "COP without frictions                       COP_w_hl    = ", COP_w_hl, " -"
    write (155,2990) "COP Real                                    COP_real    = ", COP_real, " -"
    write (155,2990), " ---------------------------------------------------------------------------- "
    write (155,2990), " Exergy analysis (Second Law analysis) "
    write (155,2990), " ---------------------------------------------------------------------------- "
    write (155,2990) "Total Mean Exergy Rate Transferred to fluid     exergy_sup_fluid    = ", exergy_sup_fluid, " W"
    write (155,2990) "Total Mean Exergy Supplied Rate (with fluid)    exergy_sup_total    = ", exergy_sup_total, " W"
    write (155,2990) "Total Mean Exergy Destroyed Rate                exergy_destroyed    = ", exergy_destroyed, " W"
    write (155,2990) "Exergy Destroyed by Throttling only             I_thr_only          = ", I_thr_only, " W"
    write (155,2990) "Exergy Destroyed by Fluid Mixing only           I_mix_only          = ", I_mix_only, " W"
    write (155,2990) "Total Mean Exergy Destroyed Rate (Friction)     I_friction_total    = ", I_friction_total, " W"
    write (155,2990) "Total Mean Exergy Destroyed Rate (Throttling)   I_thr_total         = ", I_thr_total, " W"
    write (155,2990) "Total Mean Exergy Destroyed Rate (Heat)         I_heat_total        = ", I_heat_total, " W"
    write (155,2990) "Total Mean Exergy Destroyed Rate (Reexpansion)  I_reexpand          = ", I_reexpand, " W"
    write (155,*) ' '
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,*), " Efficiencies"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,2990) "2nd Law Efficiency                          eff_2nd     = ", eff_2nd*100," %"
    write (155,2990) "Motor Efficiency                            eff_motor   = ", eff_motor*100," %"
    write (155,2990) "Compression Efficiency                      eff_comp    = ", eff_comp*100," %"
    write (155,2990) "Volumetric Efficiency                       eff_vol     = ", eff_vol*100," %"
    write (155,2990) "Volumetric Efficiency II (disc/suct)        eff_vol_2   = ", eff_vol_2*100," %"
    write (155,2990) "Mechanical Efficiency                       eff_mec     = ", eff_mec*100," %"
    write (155,2990) "Overall Efficiency                          eff_overall = ", eff_overall*100," %"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,*), " Torque profile in NVC"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,2990) "Average Input Torque                    T_avrg_input    = ", T_avrg_input, " Nm"
    write (155,2990) "Average Torque without loss             T_avrg_no_loss  = ", T_avrg_no_loss, " Nm"
    write (155,2990) "Average Torque of frictional loss       T_avrg_loss     = ", T_avrg_loss, " Nm"
    write (155,2990) "Average Rotor Inertia Torque            T_avrg_I_ro     = ", T_avrg_I_ro, " Nm"
    write (155,2990) "Average Vane Housing Inertia Torque     T_avrg_I_vh     = ", T_avrg_I_vh, " Nm"
    write (155,2990) "Average Compression Torque              T_avrg_com      = ", T_avrg_com, " Nm"
    write (155,2990) "Average Bearing Friction Torque         T_avrg_bear_s   = ", T_avrg_bear_s, " Nm"
    write (155,2990) "Average Eccentric Friction Torque       T_avrg_ecc      = ", T_avrg_ecc, " Nm"
    write (155,2990) "Maximum Shaft Torque                    T_peak          = ", T_peak, " Nm"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,*), " Power profile of NVC"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,2990) "Average Power Input from motor      P_input_motor       = ", P_input_motor, " W"
    write (155,2990) "Average Power Input to compressor   P_input             = ", P_input_opt, " W"
    write (155,2990) "Average Power without Loss          P_avrg_no_loss      = ", P_avrg_no_loss_opt, " W"
    write (155,2990) "Average Frictional Power Loss       P_avrg_loss         = ", P_avrg_loss_opt, " W"
    write (155,*)" --------------------------- Individual Power ------------------------------- "
    write (155,2990) "Average Power Input                 P_input             = ", P_input_opt, " W"
    write (155,2990) "Average Power without Loss          P_avrg_no_loss      = ", P_avrg_no_loss_opt, " W"
    write (155,2990) "Average Power with Loss             P_avrg_loss         = ", P_avrg_loss_opt, " W"
    write (155,2990) "Average Rotor Inertia Power         P_avrg_inertia_ro   = ", P_avrg_inertia_ro_opt, " W"
    write (155,2990) "Average Vane Housing Inertia Power  P_avrg_inertia_vh   = ", P_avrg_inertia_vh_opt, " W"
    write (155,2990) "Average Compression Power           P_avrg_com          = ", P_avrg_com_opt, " W"
    write (155,2990) "Average Bearing Friction loss       P_avrg_bear_s       = ", P_avrg_bear_s_opt, " W"
    write (155,2990) "Average Eccentric Friction loss     P_avrg_ecc          = ", P_avrg_ecc_opt, " W"
    write (155,2990) "Valve Power Loss                    P_valve_loss        = ", P_valve_loss_opt, " W"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,*), " Individual power losses of NVC"
    write (155,*), " ---------------------------------------------------------------------------- "
    write (155,2990) "Suction Loss                        P_loss_suc_hl       = ", P_loss_suc_hl_opt, " W"
    write (155,2990) "Discharge Loss                      P_loss_disc_hl      = ", P_loss_disc_hl_opt, " W"
    write (155,2990) "Re-expansion Loss                                       = ", L_avrg_reexpansion_opt, " W"
    write (155,*), " ============================================================================ "
    write (155,*), ' '
2989 format (2x,A,F8.4,A)
2981 format (14A25)
2982 format (F25.4, 14ES25.6)
2983 format (14ES25.6)  
2990 format (2x,A,f12.4,A)
endsubroutine