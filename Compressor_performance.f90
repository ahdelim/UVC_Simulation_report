subroutine comp_performance()
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
    include "var_exergy.f90"
    
    double precision, dimension (1:no_data+1):: p_cscv_id, p_cdcv_id
    
    
    ! -------------------------------------------
    ! Mass flow rate
    ! -------------------------------------------
    mass_total_output = mass_total - avrg_leakage_mass  ! mass_total is obtained in thermo_with_heat_leakage.f90, avrg_leakage_mass is obtained in leakage_model.f90, mass_total has already included the dead volume mass
    
    ! -------------------------------------------
    ! Cooling Capacity
    ! -------------------------------------------
    q_capacity = mass_total_output*(h_suc - h_bef_evap)       ! Cooling capacity in [W]
    
    
    ! -------------------------------------------
    ! Coefficient of Performance (COP)
    ! -------------------------------------------
    COP_ideal = q_capacity/P_ind_id             ! Ideal COP with ideal indicated power
    COP_w_o_hl = q_capacity/P_ind             ! COP without frictional power losses and without heat and leakage 
    COP_w_hl = q_capacity/P_ind_hl              ! COP with heat and leakage but no frictional loss
    COP_real = q_capacity/P_input               ! Real COP
    
    ! -------------------------------------------
    ! Efficiencies Evaluation
    ! Volumetric, Mechanical, Isentropic
    ! -------------------------------------------
    read(112,*) eff_motor
    ! ------------------------------------------
    ! Power input 
    ! ------------------------------------------
    P_input_motor = P_input/eff_motor   ! Avrg power input from motor [W]
    ! -------------------------------------------
    ! Efficiencies Evaluation
    ! Volumetric, Mechanical, Isentropic
    ! -------------------------------------------
    eff_comp = P_ind_id/(P_ind_id + P_valve_loss)
    eff_vol = (mass_total - avrg_leakage_mass)/m_flow_total_id        ! Actual_mass/theorectical_mass
    eff_vol_2 = (mass_total - avrg_leakage_mass)/mass_suct_in       ! mass discharged/mass entered
    !eff_mec = P_ind_hl/(P_ind_hl + P_avrg_loss)     ! Mechanical efficiency = Compression P/(Compression + mechanical losses) (this one by using PV diagram area)
    eff_mec = P_avrg_no_loss/P_input     ! total power used without losses over total power input           !P_avrg_com/(P_avrg_com + P_avrg_loss) ! + P_avrg_thr_loss)
    eff_overall = P_avrg_no_loss/P_input_motor  ! Overall efficiency, compression power/all losses term     !eff_comp*eff_vol*eff_mec*eff_motor
    
    
    ! -----------------------------------------------------------
    ! Show performance data on screen
    ! -----------------------------------------------------------
    print *, ' ' 
    print *, ' Showing Simulation Results ... '
    print *, " ============================================================================ "
    print *, ' Ideal compressor performance '
    print *, ' Assumptions --> Perfect mass flow, No internal leakage '
    print *, ' Assumptions --> Adiabatic, Perfect valve response'
    print *, " ---------------------------------------------------------------------------- "
    write(6,2990) "Id. Total Indicated Work for UVC          P_ind_id          = ", P_ind_id, " W"
    write(6,2990) "Id. Coefficient of Performance (COP)      COP_ideal         = ", COP_ideal, " -"
    print *, " ============================================================================ "
    print *, ' '
    print *, " ============================================================================ "
    print *, " Actual Compressor Performance "
    print *, " ---------------------------------------------------------------------------- "
    !write (113,2990) "Outer Volume                          vol_outer           = ", vol_outer/1000., " cc or cm3"
    !write (113,2990) "Inner Volume                          vol_inner           = ", vol_inner/1000., " cc or cm3"
    write (6,2990) "Total Volume                        vol_total           = ", vol_total/1000., " cc or cm3"
    write (6,2990) "Residual mass (Dead volume)         mass_residual_dead  = ", mass_residual_dead*1000, " g"
    write (6,2990) "Avrg. Leakage Mass Flow             avrg_leakage_mass   = ", avrg_leakage_mass*1000, " g/s"
    !write (113,2990) "Outer Mass Flow Rate                  mass_outer          = ", mass_outer, " kg/s"
    !write (113,2990) "Inner Mass Flow Rate                  mass_inner          = ", mass_inner, " kg/s"
    write (6,2990) "Total Actual Mass Flow Rate         mass_total_output   = ", mass_total_output, " kg/s"
    write (6,2990) "Total Actual Mass Flow Rate         mass_total_output   = ", mass_total_output*1000, " g/s"
    write (6,2990) "Total Actual Mass Flow Rate (kg/min)                    = ", mass_total_output*60, " kg/min"
    write (6,2990) "Cubic Feet per Minute (CFM)                             = ", mass_total_output/rho_disc*35.3147*60, " CFM"
    ! Cooling capacity
    !write (113,2989) "Cooling Capacity                      q_capacity          = ", q_capacity/1000., " kW"
    !write (113,2989) "Indicated Work for Outer Compressor   P_ind_1             = ", P_ind_1/1000., " kW"
    !write (113,2989) "Indicated Work for Inner Compressor   P_ind_2             = ", P_ind_2/1000., " kW"
    !write (113,2989) "Total Indicated Work for MCRC         P_ind               = ", P_ind/1000., " kW"
    write (6,2990) "Cooling Capacity                        q_capacity      = ", q_capacity, " W"
    !write (113,2990) "Indicated Work for Outer Compressor   P_ind_1             = ", P_ind_1, " W"
    !write (113,2990) "Indicated Work for Inner Compressor   P_ind_2             = ", P_ind_2, " W"
    write (6,2990) "Indicated Work w/o heat&leakage (PV)    P_ind           = ", P_ind, " W"
    write (6,2990) "Indicated Work (PV)                     P_ind_hl        = ", P_ind_hl, " W"
    print *, " ---------------------------------------------------------------------------- "
    print *, " Internal Leakage profile of NVC"
    print *, " ---------------------------------------------------------------------------- "
    write (6,2990) "Avrg. Radial Clearance Leakage        avrg_leak_rad_ro    = ", avrg_leak_rad_ro*1000, " g/s"
    write (6,2990) "Avrg. Vane Endface Leakage            avrg_leak_vef       = ", avrg_leak_vef*1000, " g/s"
    write (6,2990) "Avrg. Rotor Endface Leakage           avrg_leak_ref       = ", avrg_leak_ref*1000, " g/s"
    write (6,2990) "Avrg. Leakage Mass Flow               avrg_leakage_mass   = ", avrg_leakage_mass*1000, " g/s"
    print *, " ---------------------------------------------------------------------------- "
    print *, " Coefficienct of Performance (COP)"
    print *, " ---------------------------------------------------------------------------- "
    write (6,2990) "COP without heat, leakage and frictions     COP_w_o_hl  = ", COP_w_o_hl, " -"
    write (6,2990) "COP without frictions                       COP_w_hl    = ", COP_w_hl, " -"
    write (6,2990) "COP Real                                    COP_real    = ", COP_real, " -"
    print *, " ---------------------------------------------------------------------------- "
    print *, " Efficiencies"
    print *, " ---------------------------------------------------------------------------- "
    write (6,2990) "Motor Efficiency                            eff_motor        = ", eff_motor*100," %"
    write (6,2990) "Compression Efficiency                      eff_comp         = ", eff_comp*100," %"
    write (6,2990) "Volumetric Efficiency    (theoretical)      eff_vol          = ", eff_vol*100," %"
    write (6,2990) "Volumetric Efficiency II (disc/suct)        eff_vol_2        = ", eff_vol_2*100," %"
    write (6,2990) "Mechanical Efficiency                       eff_mec          = ", eff_mec*100," %"
    write (6,2990) "Second Law efficiency                       eff_2nd          = ", eff_2nd*100.0, " %"
    !write (6,2990) "Second Law efficiency (without fluid)       eff_2nd_wo_fluid = ", eff_2nd_wo_fluid*100.0, " %"
    write (6,2990) "Overall Efficiency                          eff_overall      = ", eff_overall*100," %"
    print *, " ---------------------------------------------------------------------------- "
    print *, " Torque profile in NVC"
    print *, " ---------------------------------------------------------------------------- "
    write (6,2990) "Average Input Torque (from motor)                       = ", T_avrg_input/0.875, " Nm"
    write (6,2990) "Average Input Torque (to compressor)    T_avrg_input    = ", T_avrg_input, " Nm"
    write (6,2990) "Average Torque without loss             T_avrg_no_loss  = ", T_avrg_no_loss, " Nm"
    write (6,2990) "Average Torque of frictional loss       T_avrg_loss     = ", T_avrg_loss, " Nm"
    print *, " Individual Torque ---------------------------------------------------------- "
    write (6,2990) "Average Rotor Inertia Torque            T_avrg_I_ro     = ", T_avrg_I_ro, " Nm"
    write (6,2990) "Average Vane Housing Inertia Torque     T_avrg_I_vh     = ", T_avrg_I_vh, " Nm"
    write (6,2990) "Average Compression Torque              T_avrg_com      = ", T_avrg_com, " Nm"
    write (6,2990) "Average Bearing Friction Torque         T_avrg_bear_s   = ", T_avrg_bear_s, " Nm"
    write (6,2990) "Average Eccentric Friction Torque       T_avrg_ecc      = ", T_avrg_ecc, " Nm"
    write (6,2990) "Average Torque of Lip Seal friction     T_avrg_lip_seal = ", T_avrg_lip_seal, " Nm"
    write (6,2990) "Maximum Shaft Torque                    T_peak          = ", T_peak, " Nm"
    print *, " ---------------------------------------------------------------------------- "
    print *, " Power profile of NVC"
    print *, " ---------------------------------------------------------------------------- "
    write (6,2990) "Average Power Input from motor      P_input_motor       = ", P_input_motor, " W"
    write (6,2990) "Average Power Input to compressor   P_input             = ", P_input, " W"
    write (6,2990) "Average Power without Loss          P_avrg_no_loss      = ", P_avrg_no_loss, " W"
    write (6,2990) "Average Frictional Power Loss       P_avrg_loss         = ", P_avrg_loss, " W"
    print *, " Individual Power ----------------------------------------------------------- "
    write (6,2990) "Average Rotor Inertia Power         P_avrg_inertia_ro   = ", P_avrg_inertia_ro, " W"
    write (6,2990) "Average Vane Housing Inertia Power  P_avrg_inertia_vh   = ", P_avrg_inertia_vh, " W"
    write (6,2990) "Average Compression Power           P_avrg_com          = ", P_avrg_com, " W"
    !write (6,2990) "Average Indicated Power             P_avrg_indicated    = ", P_avrg_indicated, " W"
    write (6,2990) "Average Bearing Friction loss       P_avrg_bear_s       = ", P_avrg_bear_s, " W"
    write (6,2990) "Average Eccentric Friction loss     P_avrg_ecc          = ", P_avrg_ecc, " W"
    !write (6,2990) "AverageThrottling Power Loss        P_avrg_thr_loss     = ", P_avrg_thr_loss, " W"
    write (6,2990) "Valve Power Loss                    P_valve_loss        = ", P_valve_loss, " W"
    print *, " ---------------------------------------------------------------------------- "
    print *, " Individual power losses of NVC"
    print *, " ---------------------------------------------------------------------------- "
    write (6,2990) "Vane Housing End-face Friction      L_avrg_ef_vh        = ", L_avrg_ef_vh, " W"
    write (6,2990) "Vane Housing Side Friction          L_avrg_s_vh         = ", L_avrg_s_vh, " W"
    write (6,2990) "Rotor-vane Side Friction            L_avrg_f_vs         = ", L_avrg_f_vs, " W"
    write (6,2990) "Rotor End-face Friction             L_avrg_ef_ro        = ", L_avrg_ef_ro, " W"
    write (6,2990) "Eccentric and rotor inner surface   L_avrg_lub          = ", L_avrg_lub, " W"
    write (6,2990) "Eccentric End-face Friction         L_avrg_ef_ecc       = ", L_avrg_ef_ecc, " W"
    write (6,2990) "Leakage loss                        L_avrg_leakage      = ", L_avrg_leakage, " W"
    write (6,2990) "Lip seal Friction                   L_avrg_lip_seal     = ", L_avrg_lip_seal, " W"
    write (6,2990) "Suction Loss w/o heat&leakage       P_loss_suc          = ", P_loss_suc, " W"
    write (6,2990) "Suction Loss                        P_loss_suc_hl       = ", P_loss_suc_hl, " W"
    write (6,2990) "Discharge Loss w/o heat&leakage     P_loss_disc         = ", P_loss_disc, " W"
    write (6,2990) "Discharge Loss                      P_loss_disc_hl      = ", P_loss_disc_hl, " W"
    write (6,2990) "Re-expansion Loss                                       = ", L_avrg_reexpansion, " W"!P_comp_loss_hl - P_comp_loss, " W"
    print *, " ============================================================================ "
    print *, ' '
    ! ---- Ideal Overview ---- !
    !write(45,2990) "Id. Indicated Work for Outer Compressor   P_ind_id_1             = ", P_ind_id, " W"
    !write(45,2990) "Id. Indicated Work for Inner Compressor   P_ind_id_2             = ", P_ind_id_2, " W"
    
    ! ----------------------------------------------------------------
    ! Write results into Overview file
    ! ----------------------------------------------------------------
    
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Actual Compressor Performance "
    write (113,*) " -------------------------------------------------------------------- "
    !write (113,2990) "Outer Volume                          vol_outer           = ", vol_outer/1000., " cc or cm3"
    !write (113,2990) "Inner Volume                          vol_inner           = ", vol_inner/1000., " cc or cm3"
    write (113,2990) "Total Volume                          vol_total           = ", vol_total/1000., " cc or cm3"
    write (113,2990) "Residual mass (Dead volume)           mass_residual_dead  = ", mass_residual_dead*1000, " g"
    write (113,2990) "Avrg. Leakage Mass Flow               avrg_leakage_mass   = ", avrg_leakage_mass*1000, " g/s"
    !write (113,2990) "Outer Mass Flow Rate                  mass_outer          = ", mass_outer, " kg/s"
    !write (113,2990) "Inner Mass Flow Rate                  mass_inner          = ", mass_inner, " kg/s"
    write (113,2990) "Total Actual Mass Flow Rate           mass_total          = ", mass_total_output, " kg/s"
    write (113,2990) "Total Actual Mass Flow Rate           mass_total          = ", mass_total_output*1000, " g/s"
    write (113,2990) "Total Actual Mass Flow Rate (kg/min)                      = ", mass_total_output*60, " kg/min"
    write (113,2990) "Cubic Feet per Minute (CFM)                               = ", mass_total_output/rho_suc*35.3147*60, " CFM"
    
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Internal Leakage profile of NVC"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2990) "Avrg. Radial Clearance Leakage        avrg_leak_rad_ro    = ", avrg_leak_rad_ro*1000, " g/s"
    write (113,2990) "Avrg. Vane Endface Leakage            avrg_leak_vef       = ", avrg_leak_vef*1000, " g/s"
    write (113,2990) "Avrg. Rotor Endface Leakage           avrg_leak_ref       = ", avrg_leak_ref*1000, " g/s"
    write (113,2990) "Avrg. Leakage Mass Flow               avrg_leakage_mass   = ", avrg_leakage_mass*1000, " g/s"

    ! Cooling capacity
    !write (113,2989) "Cooling Capacity                      q_capacity          = ", q_capacity/1000., " kW"
    !write (113,2989) "Indicated Work for Outer Compressor   P_ind_1             = ", P_ind_1/1000., " kW"
    !write (113,2989) "Indicated Work for Inner Compressor   P_ind_2             = ", P_ind_2/1000., " kW"
    !write (113,2989) "Total Indicated Work for MCRC         P_ind               = ", P_ind/1000., " kW"
    
    write (113,2990) "Cooling Capacity                          q_capacity      = ", q_capacity, " W"
    !write (113,2990) "Indicated Work for Outer Compressor   P_ind_1             = ", P_ind_1, " W"
    !write (113,2990) "Indicated Work for Inner Compressor   P_ind_2             = ", P_ind_2, " W"
    write (113,2990) "Indicated Work for NVC w/o heat & leakage P_ind           = ", P_ind, " W"
    write (113,2990) "Indicated Work                            P_ind_hl        = ", P_ind_hl, " W"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Coefficienct of Performance (COP)"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2990) "COP without heat, leakage and frictions     COP_w_o_hl    = ", COP_w_o_hl, " -"
    write (113,2990) "COP without frictions                       COP_w_hl      = ", COP_w_hl, " -"
    write (113,2990) "COP Real                                    COP_real      = ", COP_real, " -"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Efficiencies"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2990) "Motor Efficiency                          eff_motor       = ", eff_motor*100," %"
    write (113,2990) "Compression Efficiency                    eff_comp        = ", eff_comp*100," %"
    write (113,2990) "Volumetric Efficiency                     eff_vol         = ", eff_vol*100," %"
    write (113,2990) "Volumetric Efficiency II (disc/suct)      eff_vol_2       = ", eff_vol_2*100," %"
    write (113,2990) "Mechanical Efficiency                     eff_mec         = ", eff_mec*100," %"
    write (113,2990) "Second Law efficiency                     eff_2nd         = ", eff_2nd*100.0, " %"
    !write (113,2990) "Second Law efficiency (without fluid)     eff_2nd_wo_fluid  = ", eff_2nd_wo_fluid*100.0, " %"
    write (113,2990) "Overall Efficiency                        eff_overall     = ", eff_overall*100," %"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Torque profile of NVC"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2990) "Average Input Torque (from motor)                         = ", T_avrg_input/0.875, " Nm"
    write (113,2990) "Average Input Torque                    T_avrg_input      = ", T_avrg_input, " Nm"
    write (113,2990) "Average Torque without loss             T_avrg_no_loss    = ", T_avrg_no_loss, " Nm"
    write (113,2990) "Average Frictional Torque               T_avrg_loss       = ", T_avrg_loss, " Nm"
    write (113,*) " Individual Torque -------------------------------------------------- "
    write (113,2990) "Average Rotor Inertia Torque            T_avrg_I_ro       = ", T_avrg_I_ro, " Nm"
    write (113,2990) "Average Vane Housing Inertia Torque     T_avrg_I_vh       = ", T_avrg_I_vh, " Nm"
    write (113,2990) "Average Compression Torque              T_avrg_com        = ", T_avrg_com, " Nm"
    write (113,2990) "Average Bearing Friction Torque         T_avrg_bear_s     = ", T_avrg_bear_s, " Nm"
    write (113,2990) "Average Eccentric Friction Torque       T_avrg_ecc        = ", T_avrg_ecc, " Nm"
    write (113,2990) "Average Torque of Lip Seal friction     T_avrg_lip_seal   = ", T_avrg_lip_seal, " Nm"
    write (113,2990) "Maximum Shaft Torque                    T_peak            = ", T_peak, " Nm"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Power profile of NVC"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2990) "Average Power Input from motor      P_input_motor         = ", P_input_motor, " W"
    write (113,2990) "Average Power Input to compressor   P_input               = ", P_input, " W"
    write (113,2990) "Average Power without Loss          P_avrg_no_loss        = ", P_avrg_no_loss, " W"
    write (113,2990) "Average Frictional Loss Power       P_avrg_loss           = ", P_avrg_loss, " W"
    write (113,*) " Individual Power --------------------------------------------------- "
    write (113,2990) "Average Rotor Inertia Power         P_avrg_inertia_ro     = ", P_avrg_inertia_ro, " W"
    write (113,2990) "Average Vane Housing Inertia Power  P_avrg_inertia_vh     = ", P_avrg_inertia_vh, " W"
    write (113,2990) "Average Compression Power           P_avrg_com            = ", P_avrg_com, " W"
    write (113,2990) "Average Bearing Friction loss       P_avrg_bear_s         = ", P_avrg_bear_s, " W"
    write (113,2990) "Average Eccentric Friction loss     P_avrg_ecc            = ", P_avrg_ecc, " W"
    write (113,2990) "AverageThrottling Power Loss        P_avrg_thr_loss       = ", P_avrg_thr_loss, " W"
    write (113,2990) "Valve Power Loss                    P_valve_loss          = ", P_valve_loss, " W"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Individual power losses of NVC"
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2990) "Vane Housing End-face Friction      L_avrg_ef_vh          = ", L_avrg_ef_vh, " W"
    write (113,2990) "Vane Housing Side Friction          L_avrg_s_vh           = ", L_avrg_s_vh, " W"
    write (113,2990) "Rotor-vane Side Friction            L_avrg_f_vs           = ", L_avrg_f_vs, " W"
    write (113,2990) "Rotor End-face Friction             L_avrg_ef_ro          = ", L_avrg_ef_ro, " W"
    write (113,2990) "Eccentric and rotor inner surface   L_avrg_lub            = ", L_avrg_lub, " W"
    write (113,2990) "Eccentric End-face Friction         L_avrg_ef_ecc         = ", L_avrg_ef_ecc, " W"
    write (113,2990) "Leakage loss                        L_avrg_leakage        = ", L_avrg_leakage, " W"
    write (113,2990) "Lip seal Friction                   L_avrg_lip_seal       = ", L_avrg_lip_seal, " W"
    write (113,2990) "Suction Loss w/o heat&leakage       P_loss_suc            = ", P_loss_suc, " W"
    write (113,2990) "Suction Loss                        P_loss_suc_hl         = ", P_loss_suc_hl, " W"
    write (113,2990) "Discharge Loss w/o heat&leakage     P_loss_disc           = ", P_loss_disc, " W"
    write (113,2990) "Discharge Loss                      P_loss_disc_hl        = ", P_loss_disc_hl, " W"
    write (113,2990) "Re-expansion Loss                                         = ", L_avrg_reexpansion, " W"!P_comp_loss_hl - P_comp_loss, " W"
    write (113,*) " ==================================================================== "
    
    ! ---- Ideal Overview ---- !
    !write(45,2990) "Id. Indicated Work for Outer Compressor   P_ind_id_1             = ", P_ind_id, " W"
    !write(45,2990) "Id. Indicated Work for Inner Compressor   P_ind_id_2             = ", P_ind_id_2, " W"
    write(45,2990) "Id. Total Indicated Work for MCRC         P_ind_id              = ", P_ind_id, " W"
    write(45,2990) "Id. Coefficient of Performance (COP)      COP_ideal             = ", COP_ideal, " -"
    
2981 format (14A25)
2982 format (F25.4, 14ES25.6)
2983 format (14ES25.6)  
2989 format (2x,A,F8.4,A)
2990 format (2x,A,f12.4,A)
     
endsubroutine