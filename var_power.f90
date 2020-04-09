! include "var_power.f90"
    
    ! ---------- output variables ----------- !
    double precision, dimension (1:no_data+1) :: L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, L_ef_ecc       ! Losses definition
    double precision, dimension (1:no_data+1) :: L_ef_vro, L_ef_rohc        ! component losses of rotor (vane and body)
    double precision, dimension (1:no_data+1) :: L_reexpansion, L_leakage
    double precision, dimension (1:no_data+1) :: L_lip_seal     ! frictional loss due to lip seal
    double precision, dimension (1:no_data+1) :: P_inst_total, P_inst_total_no_loss, P_f_loss, P_bear_s, P_thr_loss, P_indicated     ! total instantaneous power, instantaneous power without friction and instantaneous total frictional power loss
    double precision, dimension (1:no_data+1) :: P_inertia_ro, P_inertia_vh, P_com      ! individual power, rotor and vane housing inertia, compression power
    double precision, dimension (1:no_data+1) :: T_loss, T_lip_seal                         ! frictional torque due to frictional loss, lip seal
    double precision P_input_motor, P_input, P_avrg_no_loss, P_avrg_loss, P_avrg_inertia_ro, P_avrg_inertia_vh, P_avrg_com, P_valve_loss, P_avrg_bear_s, P_avrg_ecc, P_avrg_thr_loss, P_avrg_indicated        ! power input based on average instantaneous power, and other average powers
    common/power_model/P_input_motor, P_input, P_avrg_no_loss, P_avrg_loss, P_avrg_inertia_ro, P_avrg_inertia_vh, P_avrg_com, P_valve_loss, P_avrg_bear_s, P_avrg_ecc, P_avrg_thr_loss, P_avrg_indicated
    double precision L_avrg_ef_vh, L_avrg_s_vh, L_avrg_f_vs, L_avrg_ef_ro, L_avrg_lub, L_avrg_ef_ecc, L_avrg_reexpansion, L_avrg_lip_seal, L_avrg_leakage
    common/individual_loss/L_avrg_ef_vh, L_avrg_s_vh, L_avrg_f_vs, L_avrg_ef_ro, L_avrg_lub, L_avrg_ef_ecc, L_avrg_reexpansion, L_avrg_lip_seal, L_avrg_leakage