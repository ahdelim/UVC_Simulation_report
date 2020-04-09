
    double precision omega_natural_dv, omega_natural_dv_back
    double precision F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start_dv, phi_mode_mid_dv, phi_mode_end_dv, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back
    double precision omega_natural_dv_rec, omega_natural_dv_back_rec
    double precision I_dv_1, A_cross_dv_1, beta_r_1     ! to calculate natural freq for rectangular valve
    common/valve_natural_freq/omega_natural_dv, omega_natural_dv_back
    common/valve_mode_shape/F_up_dv, F_down_dv, F_up_dv_back, F_down_dv_back, phi_mode_start_dv, phi_mode_mid_dv, phi_mode_end_dv, phi_mode_start_back, phi_mode_mid_back, phi_mode_end_back
    double precision F_up_dv_rec, F_up_dv_back_rec, F_down_dv_rec, F_down_dv_back_rec, phi_mode_start_rec, phi_mode_mid_rec, phi_mode_start_back_rec, phi_mode_mid_back_rec, A_cross_rec    ! rec valve
    common/rec_disc_valve_F/F_up_dv_rec, F_up_dv_back_rec, F_down_dv_rec, F_down_dv_back_rec, phi_mode_start_rec, phi_mode_mid_rec, phi_mode_start_back_rec, phi_mode_mid_back_rec, A_cross_rec
    common/rec_valve_nat_freq/omega_natural_dv_rec, omega_natural_dv_back_rec
    
    ! include "var_reed_valve.f90"