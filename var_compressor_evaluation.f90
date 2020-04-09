    double precision m_flow_total_id, q_capacity_id                     ! Ideal mass flow and cooling capacity
    double precision h_bef_evap, vol_total, mass_total, q_capacity, mass_suct_in, mass_total_output      ! Real mass flow and cooling 
    double precision COP_w_o_hl, COP_w_hl, COP_ideal, COP_real                       ! COP 
    double precision eff_comp, eff_vol, eff_vol_2, eff_mec, eff_motor, eff_overall                                            ! Efficiencies to evaluate compressor
    double precision eff_2nd, eff_2nd_wo_fluid        ! 2nd law efficiency
    
    common/cooling_capacity/h_bef_evap, vol_total, mass_total, q_capacity, mass_suct_in, mass_total_output
    common/efficiency/COP_w_o_hl, COP_w_hl, COP_ideal, COP_real, eff_comp, eff_vol, eff_vol_2, eff_mec, eff_motor, eff_overall, eff_2nd, eff_2nd_wo_fluid
    common/ideal_preformance/m_flow_total_id, q_capacity_id
    
    ! include "var_compressor_evaluation.f90"