!! ----------------------------------------------------
!! The Genetic Algorithm main body
!! Randomly generate variables of 
!! --------------------------------------
!
!subroutine GA_Optimization_main()
!    implicit none
!    include "var_GA_optimization.f90"       ! <------ put your array/variables declaration in this subroutine
!    integer i
!    
!    ! ================================
!    ! Optimization start -->
!    ! A population of a number of max_population will be randomly generated
!    ! Variables set 1 - r_oc, ratio_ro_cyl, r_shaft, l_bearing, dia_suc, dia_disc
!    ! ================================
!    
!    ! --------------------------------
!    ! constraints set for each variables are as below
!    ! ***** Please make these constraints into a input file    (.txt)
!    ! r_oc          -   30 ~ 50
!    ! ratio_ro_cyl  -   0.6 ~ 0.9
!    ! r_shaft       -   6 ~ 18
!    ! l_bearing     -   10 ~ 45
!    ! dia_suc       -   5 ~ 20
!    ! dia_disc      -   4 ~ 15
!    ! --------------------------------
!    
!    ! --------------------------------
!    ! G.A. Part 0 ----> Initialize some constrains
!    ! note: i put one in GA_geo_constraint_check.f90
!    ! --------------------------------
!    vol_max_GA = 34.0 ![cc]   --> make this in the constraint file together
!    vol_max_GA = vol_max_GA*1000.0 ! convert [cc] to [mm3]
!    ! ----------------------------------
!    ! G.A. Part 1 ----> random individuals
!    ! ----------------------------------
!    
!    do i = 1, population_number ! 1 to population number, as a parameter on top or as an input in input file (.txt)
!        ! ----------------------------------------
!        ! To randomly generate the 6 variables
!        ! These 6 variables will go to geometrical constraint checking
!        ! if everything is satisfied, then save this set
!        ! else, regenerate another set and go into the constraint checking again
!        ! ----------------------------------------
!        
!        ! ******************************************************    
!        ! Your subroutine input here
!        ! for regenerating, you may modify in the constraint there
!        ! **************************
!        
!        
!        ! ---------------------------
!        ! Geometrical Constraint checking
!        ! ---------------------------
!        call GA_geo_constraint_check(var1, var2, var3) ! only r_oc, ratio_ro_cyl, r_shaft are required to be checked, u may replace var1,var2,var3
!        
!    
!    enddo
!    
!    ! --------------------------------
!    ! Allocate array
!    ! These arrays were used in complex optimization method
!    ! the opt_ori_complex is prompted to user for input, and afterthat allocate the size of array using the input value
!    ! in G.A., it can be number of individual in population (?)
!    ! --------------------------------
!
!    !allocate(ori_r_oc(opt_ori_complex), ori_l_com(opt_ori_complex), ori_r_shaft(opt_ori_complex), ori_l_vh(opt_ori_complex), ori_w_vs_ro(opt_ori_complex), ori_w_v_ro(opt_ori_complex), ori_dia_suc(opt_ori_complex), ori_dia_disc(opt_ori_complex))
!    !allocate(ori_r_oc_2(opt_ori_complex), ori_r_ro_2(opt_ori_complex), ori_r_shaft_2(opt_ori_complex), ori_l_bearing_2(opt_ori_complex), ori_dia_disc_2(opt_ori_complex), ori_t_dv_2(opt_ori_complex))
!    !allocate(a_P_ind_hl_opt(opt_ori_complex), a_P_ind_id_opt(opt_ori_complex), a_P_input_opt(opt_ori_complex), a_P_avrg_no_loss_opt(opt_ori_complex), a_P_avrg_loss_opt(opt_ori_complex), a_P_avrg_inertia_ro_opt(opt_ori_complex), a_P_avrg_inertia_vh_opt(opt_ori_complex), a_P_avrg_com_opt(opt_ori_complex), a_P_valve_loss_opt(opt_ori_complex), a_P_avrg_bear_s_opt(opt_ori_complex),a_P_avrg_ecc_opt(opt_ori_complex))
!    !allocate(a_T_peak_opt(opt_ori_complex), a_L_avrg_ef_vh_opt(opt_ori_complex), a_L_avrg_s_vh_opt(opt_ori_complex), a_L_avrg_f_vs_opt(opt_ori_complex), a_L_avrg_ef_ro_opt(opt_ori_complex), a_L_avrg_lub_opt(opt_ori_complex),a_L_avrg_reexpansion_opt(opt_ori_complex))
!    !allocate(a_obj_opt(opt_ori_complex), a_eff_2nd(opt_ori_complex), a_eff_mec(opt_ori_complex), a_eff_vol(opt_ori_complex), a_COP_real(opt_ori_complex))
!    
!    ! ----------------------------------
!    ! G.A. Part 2 ----> selection (?)
!    ! ----------------------------------
!    ! After generated the required set of variables in Part 1
!    ! This do-loop use the generate set to calculate for the fitness
!    ! ----------------------------------
!    do 1818 iteration = 1, xxxxx ! <------ rmb to change this
!            ! ----- Variables from Part 1
!            call GA_main_dimension(var1,var2,var3,var4,var5,var6)
!            ! ----- mathematical model
!            call GA_thermo_main()
!            ! ----- Compressor performance evaluation
!            call optimization_performance()
!            ! -------------------------------
!            ! Assign points to array
!            ! You have to take all these into your results as all of these affect the efficiencies
!            ! -------------------------------
!            a_P_ind_hl_opt(opt_i) = P_ind_hl_opt
!            a_P_ind_id_opt(opt_i) = P_ind_id_opt
!            a_P_input_opt(opt_i) = P_input_opt
!            a_P_avrg_no_loss_opt(opt_i) = P_avrg_no_loss_opt 
!            a_P_avrg_loss_opt(opt_i) = P_avrg_loss_opt
!            a_P_avrg_inertia_ro_opt(opt_i) = P_avrg_inertia_ro_opt
!            a_P_avrg_inertia_vh_opt(opt_i) = P_avrg_inertia_vh_opt
!            a_P_avrg_com_opt(opt_i) = P_avrg_com_opt
!            a_P_valve_loss_opt(opt_i) = P_valve_loss_opt
!            a_P_avrg_bear_s_opt(opt_i) = P_avrg_bear_s_opt
!            a_P_avrg_ecc_opt(opt_i) = P_avrg_ecc_opt
!            a_T_peak_opt(opt_i) = T_peak_opt
!            a_L_avrg_ef_vh_opt(opt_i) = L_avrg_ef_vh_opt
!            a_L_avrg_s_vh_opt(opt_i) = L_avrg_s_vh_opt
!            a_L_avrg_f_vs_opt(opt_i) = L_avrg_f_vs_opt
!            a_L_avrg_ef_ro_opt(opt_i) = L_avrg_ef_ro_opt
!            a_L_avrg_lub_opt(opt_i) = L_avrg_lub_opt
!            a_L_avrg_reexpansion_opt(opt_i) = L_avrg_reexpansion_opt
!            a_COP_real(opt_i) = COP_real
!            a_eff_mec(opt_i) = eff_mec
!            a_eff_2nd(opt_i) = eff_2nd
!            a_eff_vol(opt_i) = eff_vol
!            
!    print *, ' --------------------------------------------------------------------------- '
!    print'(1x,A,5F10.4)', " COP array: ", a_COP_real
!    print *, ' ---------------------------------- '
!    print'(1x,A,5F10.4)', " eff_mec array: ", a_eff_mec
!    print *, ' ---------------------------------- '
!    print'(1x,A,5F10.4)', " eff_2nd array: ", a_eff_2nd
!    print *, ' ---------------------------------- '
!    print'(1x,A,5F10.4)', " eff_vol array: ", a_eff_vol
!    print *, ' --------------------------------------------------------------------------- '
!1818 continue    
!    
!endsubroutine optimization_main