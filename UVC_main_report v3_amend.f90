
    program UVC_simulation

    ! Body of UVC_simulation
    ! ====================================================================
    ! |             Simulation of U-Vane Compressor                  |
    ! ====================================================================
    
    ! ----------------------------------------    Declaring Variables    ---------------------------------------------- !
    implicit none
    integer i
    integer switch_valve
    common/valve_case/switch_valve
    include "var_optimization.f90"
    include "Declaring_variables.f90"
    ! ================================================================================================================= !
    !    Simulation Body    
    ! ================================================================================================================= !
    
    !print *, ' '
    !print *, ' ------------------------ UVC Simulation ------------------------ '
        
    ! ---------------------------------------------------------------------------- !
    ! record time 
    ! ---------------------------------------------------------------------------- !
    call SYSTEM_CLOCK(count,count_rate)
    call CPU_TIME(CPU_start)
    
    ! ---------------------------------------------------------------------------- !
    ! Open Files
    ! ---------------------------------------------------------------------------- !
    call open_file
    !print *, ' Creating files                                     ...Done !'
    !print *, ' '
    write (113,*) ' '
    write (113,*) " ==================================================================== "
    write (113,*) '                  U-Vane Compressor Overall Information  '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) ' '
    
    call UVC_simulation_info
    ! ---------------------------------------------------------------------------- !
    ! Options model (1 = ON, 0 = OFF)
    ! Select the model that is going to run for the simulation
    ! ---------------------------------------------------------------------------- !
    read (115,*) switch_valve         ! this is for the valve shape, switch_valve = 2 meaning using rectangular valve
    read (115,*) journal_opt        ! if journal_opt is 1, means the model is "ON", otherwise "OFF"
    read (115,*) heat_opt           
    read (115,*) leakage_opt
    read (115,*) oil_network_opt
    read (115,*) exergy_opt
    read (115,*) optimization_opt
    read (115,*) optimization_GA_opt
    close(115)
    if (journal_opt == 1) then
        print *, ' >>>> Journal Bearing Model            ...turned ON!'
    else
        print *, ' >>>> Journal Bearing Model            ...turned OFF!'
    endif
    if (heat_opt == 1) then
        print *, ' >>>> Heat Trasnfer Model              ...turned ON!'
    else
        print *, ' >>>> Heat Trasnfer Model              ...turned OFF!'
    endif
    if(leakage_opt == 1) then
        print *, ' >>>> Internal Leakage Model           ...turned ON!'
    else
        print *, ' >>>> Internal Leakage Model           ...turned OFF!'
    endif
    if(oil_network_opt == 1) then
        print *, ' >>>> Oil Lubricant Network Model      ...turned ON!'
    else
        print *, ' >>>> Oil Lubricant Network Model      ...turned OFF!'
    endif
    if (exergy_opt == 1) then
        print *, ' >>>> Exergy Model                     ...turned ON!'
    else
        print *, ' >>>> Exergy Model                     ...turned ON!'
    endif
    if(optimization_opt == 1) then
        print *, ' -------------------------------------------------- '
        print *, ' Optimization                          ...turned ON! '
        print *, ' -------------------------------------------------- '
        call optimization_obj_select(obj_func_input, obj_var_set)
        opt_i = 0
    else
        print *, ' >>>> Optimization                     ...turned OFF! '
    endif
    if(optimization_GA_opt == 1) then
        print *, ' -------------------------------------------------- '
        print *, ' Optimization G.A.                     ...turned ON! '
        print *, ' -------------------------------------------------- '
        !call optimization_obj_select(obj_func_input, obj_var_set)
        !opt_i = 0
    else
        print *, ' >>>> Optimization G.A.                ...turned OFF! '
    endif
    
    print *,'  '
    ! ---------------------------------------------------------------------------- !
    ! REFPROP Initialization
    ! ---------------------------------------------------------------------------- !
    call refprop_ini(fluid)
    
    ! ---------------------------------------------------------------------------- !
    ! Operational Parameters / Materials properties / Constants
    ! ---------------------------------------------------------------------------- !
    call operational_parameter
    print *, ' >>>> Operational parameters, materials properties       ...Input Done !'
    
    ! ---------------------------------------------------------------------------- !
    ! Simulation Parameters
    ! ---------------------------------------------------------------------------- !
    call simulation_parameter       ! Step size
    print *, ' >>>> Simulation parameters                              ...Input Done !'
    
    ! ---------------------------------------------------------------------------- !
    ! Main Design Dimensions
    ! ---------------------------------------------------------------------------- !
    call main_dimension
    print *, ' >>>> Compressor dimensions                              ...Input Done !'
    print *, ' '
    
    ! ---------------------------------------------------------------------------- !
    ! Geometrical Model
    ! ---------------------------------------------------------------------------- !
    print *, ' >>>> Geometrical model...'
    call geo_m(theta_1, theta_2, l_v, v_com, v_suc, gamma_1)
    
    ! Time used to calculate geometrical model
    call CPU_TIME(CPU_geo)
    write (6,2988), " CPU Time elapsed for Geometrical model           = ", (CPU_geo - CPU_start), " seconds"
    print *," ============================================================================ "
    print *, ' '
    
    ! ---------------------------------------------------------------------------- !
    ! Suction and discharge condition
    ! ---------------------------------------------------------------------------- !
    call working_fluid_operating_condition(fluid(1))
    !print *, ' Compressor operating conditions                    ...Input Done !'
    print *, ' '

    call CPU_TIME(CPU_condition)
    write (6,2988), " CPU Time elapsed for Setting Up Simulation       = ", (CPU_condition - CPU_geo), " seconds"
    print *," ============================================================================ "
    print *, ' '
    ! ---------------------------------------------------------------------------- !
    ! Kinematic Model
    ! ---------------------------------------------------------------------------- !
    print *, ' >>>> Kinematic model...'
    call kinematics_m(theta_1, gamma_1, l_v, dlvdt, dlvdtt, dgammadt_1, dgammadtt_1, dvdtheta1_suc, dvdtheta1_com)
    
    call CPU_TIME(CPU_kine)
    write (6,2988), " CPU Time elapsed for Kinematic model             = ", (CPU_kine - CPU_geo), " seconds"
    print *," ============================================================================ "
    print *, ' '
    ! ---------------------------------------------------------------------------- !
    ! Ideal Thermodynamic Model
    ! ---------------------------------------------------------------------------- !
    print *, ' >>>> Ideal Thermnodynamic model...'
    call thermo_com_id(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id)
    call CPU_TIME(CPU_thermo_id)
    write (6,2988), " CPU Time elapsed for Ideal Thermodynamic model   = ", (CPU_thermo_id - CPU_kine), " seconds"
    print *," ============================================================================ "
    print *, ' '
    ! ---------------------------------------------------------------------------- !
    ! Thermodynamic Model
    ! ---------------------------------------------------------------------------- !
    print *, ' >>>> Real Thermnodynamic model...'
    call thermo_m(theta_1, theta_2, dlvdt, dgammadt_1, v_com, v_suc, l_v, p_cscv_id, p_cdcv_id, dvdtheta1_s, dvdtheta1_d, dqdtheta_sc, dqdtheta_dc, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
    call CPU_TIME(CPU_thermo)
    write (6,2988), " CPU Time elapsed for Thermodynamic model         = ", (CPU_thermo - CPU_thermo_id), " seconds"
    print *," ============================================================================ "
    print *, ' '
    ! ---------------------------------------------------------------------------- !
    ! Dynamic Model
    ! ---------------------------------------------------------------------------- !
    print *, ' >>>> Dynamic model...'
    call dynamic_m(theta_1, theta_2, gamma_1, l_v, p_scv, p_dcv, dgammadtt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, T_inertia_ro, T_inertia_vh, T_com_C)
    call CPU_TIME(CPU_dynamic)
    write (6,2988), " CPU Time elapsed for Dynamic model               = ", (CPU_dynamic - CPU_thermo), " seconds"
    print *," ============================================================================ "
    print *, ' '
    ! ---------------------------------------------------------------------------- !
    ! Journal Bearing (Hirani)
    ! ---------------------------------------------------------------------------- !
    if (journal_opt == 1) then
        print *, ' >>>> Journal Bearing model...'
        call journal_bear_hirani(theta_1, F_cx, F_cy, F_resultant, eccr, att, h_min, p_max, Q_martin)
        ! ---------------------
        ! Write into files
        ! ---------------------
        write (94,2981) 'Degree','h_min[micron-m]','p_max[MPa]','Q_martin[cc/s]','Eccen','Att.Angle[degree]'
        do i = 1,no_data+data_step,data_step
            write (94,2982) theta_1(i)*180./pi,h_min(i)*1.d6,p_max(i)/1.d6,Q_martin(i)*1.d6,eccr(i),att(i)
        enddo
        call CPU_TIME(CPU_journal)
        write (6,2988), " CPU Time elapsed for Journal Bearing model       = ", (CPU_journal - CPU_dynamic), " seconds"
        print *," ============================================================================ "
        print *, ' '
    else
        do i = 1, no_data+1
            eccr(i) = 0.0   ! in case oil network model use this
        enddo
        call CPU_TIME(CPU_journal)
    endif
    
    ! ---------------------------------------------------------------------------- !
    ! Oil Lubrication Network Model (Kok Ming, H.J. Kim)
    ! ---------------------------------------------------------------------------- !
    if (oil_network_opt == 1) then
        print *, ' >>>> Oil Lubricant Network model...'
        !call oil_lub_network(theta_1, theta_2, p_dcv, p_scv, eccr)     ! with upper bearing groove
        !call oil_lub_network_ver4(theta_1, theta_2, p_dcv, p_scv, eccr)     ! without upper bearing groove
        call oil_lub_network_ver4_1(theta_1, theta_2, p_dcv, p_scv, eccr)     ! without upper bearing groove
        call CPU_TIME(CPU_oil_lub)
        write (6,2988), " CPU Time elapsed for Oil Lubrication Network model       = ", (CPU_oil_lub - CPU_journal), " seconds"
        print *," ============================================================================ "
        print *, ' '
    else
        call CPU_TIME(CPU_oil_lub)
    endif
    
    ! ---------------------------------------------------------------------------- !
    ! Power loss model
    ! ---------------------------------------------------------------------------- !
    print *, ' >>>> Power model...'
    call power_m(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, h_scv, h_dcv, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, dvdtheta1_s, dvdtheta1_d, dlvdt, dgammadt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, eccr, att, T_inertia_ro, T_inertia_vh, T_com_C, L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s, L_lip_seal)
    call CPU_TIME(CPU_power)
    write (6,2988), " CPU Time elapsed for Power model                 = ", (CPU_power - CPU_oil_lub), " seconds"
    print *," ============================================================================ "
    print *, ' '
    
    ! ---------------------------------------------------------------------------- !
    ! Second Law analysis (Exergy)
    ! ---------------------------------------------------------------------------- !
    if (exergy_opt == 1) then
        print *, ' >>>> Exergy model...'
        call exergy_main(theta_1, v_com, v_suc, p_scv, p_dcv, t_scv, t_dcv, s_scv, s_dcv, h_scv, h_dcv, u_scv, u_dcv, m_scv, m_dcv, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, dqdtheta_sc, dqdtheta_dc, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex, L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s, L_lip_seal)
        call CPU_TIME(CPU_exergy)
        write (6,2988), " CPU Time elapsed for Exergy model       = ", (CPU_exergy - CPU_power), " seconds"
        print *," ============================================================================ "
        print *, ' '
    else
        call CPU_TIME(CPU_exergy)
    endif
    
    
    ! ---------------------------------------------------------------------------- !
    ! Compressor Performance
    ! ---------------------------------------------------------------------------- !
    call comp_performance()
    
    ! ----------------------------------------------------------------------------
    ! Elapsed Time
    ! ---------------------------------------------------------------------------- !
    call SYSTEM_CLOCK(count_total)
    call CPU_TIME(CPU_finish)
    write (6,2988),     " Total CPU Time elapsed                           = ", (CPU_finish - CPU_start), " seconds"
    if ((count_total - count)/count_rate > 60) then
        count_minute = ((count_total - count)/count_rate)/60
        write (6,2989), " Total Time elapsed                               = ", count_minute, " minutes", ((count_total - count)/count_rate) - count_minute*60, " seconds"
    else
        write (6,2988), " Total Time elapsed                               = ", (count_total - count)/count_rate, " seconds"
    endif
    
    print *, ' --------------------------------------------------------------------------- '
    print *,"                           END OF SIMULATION           "
    print *," ============================================================================ "
    pause

    ! --------------------------------------------------------------
    ! Optimization algorithm starts here
    ! A original pool of random geometrical variables will be generated (Original Complex)
    ! Followed by reflection of worst point to best point
    ! Set 1 - !  r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
    ! Set 2 - !  r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv
    ! --------------------------------------------------------------
    if (optimization_opt == 1) then
        call CPU_TIME(CPU_optimization)
        print *," ============================================================================ "
        print*, '                    CONSTRAINTS OPTIMIZATION STARTS  '
        print *, ' --------------------------------------------------------------------------- '
        call Optimization_main() !  r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
        !print*, ' TESTING 123123123123123123 ' 
        call SYSTEM_CLOCK(count_total)
        call CPU_TIME(CPU_optimization_finish)
        write (6,2988),     " Total CPU Optimization Time elapsed              = ", (CPU_optimization_finish - CPU_optimization), " seconds"
        if ((count_total - count)/count_rate > 60) then
            count_minute = ((count_total - count)/count_rate)/60
            write (6,2989), " Total Time elapsed                               = ", count_minute, " minutes", ((count_total - count)/count_rate) - count_minute*60, " seconds"
        else
            write (6,2988), " Total Time elapsed                               = ", (count_total - count)/count_rate, " seconds"
        endif
        print *, ' --------------------------------------------------------------------------- '
        print *,"                           END OF OPTIMIZATION           "
        print *," ============================================================================ "
        pause
    endif
    
    ! --------------------------------------------------------------
    ! Optimization with G.A. starts here
    ! The stronger of among a population of randomly generated individuals will be selected
    ! The selection method will be done by G.A. selective algorithm
    ! Variables Set 1 - !  r_oc, ratio_ro_cyl, r_shaft, l_bearing, dia_suc, dia_disc
    ! --------------------------------------------------------------
    
    !if (optimization_GA_opt == 1) then
    !    call CPU_TIME(CPU_optimization_GA)
    !    print *," ============================================================================ "
    !    print*, '                    G.A. OPTIMIZATION STARTS  '
    !    print *, ' --------------------------------------------------------------------------- '
    !    call GA_Optimization_main() !  
    !    call SYSTEM_CLOCK(count_total)
    !    call CPU_TIME(CPU_optimization_GA_end)
    !    write (6,2988),     " Total CPU Optimization Time elapsed              = ", (CPU_optimization_GA_end - CPU_optimization_GA), " seconds"
    !    if ((count_total - count)/count_rate > 60) then
    !        count_minute = ((count_total - count)/count_rate)/60
    !        write (6,2989), " Total Time elapsed                               = ", count_minute, " minutes", ((count_total - count)/count_rate) - count_minute*60, " seconds"
    !    else
    !        write (6,2988), " Total Time elapsed                               = ", (count_total - count)/count_rate, " seconds"
    !    endif
    !    print *, ' --------------------------------------------------------------------------- '
    !    print *,"                           END OF G.A. OPTIMIZATION           "
    !    print *," ============================================================================ "
    !    pause
    !endif
    
    
    
    
    
    
    
    
2988 format (1x,A,F15.4,A)
2989 format (1x,A,I5,A,F5.1,A)
2981 format (15A25)
2982 format (F25.4, 14ES25.6)    
2983 format (14ES25.6) 
2990 format (2x,A,F12.4,A)  
end program UVC_simulation

