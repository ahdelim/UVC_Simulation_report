
    double precision rev                         ! Operating speed in rpm
    double precision omega_1, alpha_1, freq         ! Operating Speed, acceleration, frequency = rev/60
    double precision omega_b, rev_b
    double precision coef_sucport, coef_discport      ! coefficient of discharge
    double precision coef_vef, coef_ref, coef_rad       ! coefficient for leakage flow to account for oil flow
    double precision rho_dv, E_dv, dpratio_dv, coef_dv, coef_dv_back, sur_tension_oil, angle_oil_film, ratio_val_port        ! Valve reed density, Young's Modulus, damping ratio
    double precision T_0, p_0       ! atmophere temp and pressure
    double precision T_oil_sump, T_oil_flow     ! Oil reservoir temperature (constant) and flowing oil temperature
    double precision coef_vs, coef_vhs, coef_circ_lip                ! Vane side frictional coefficient (coef_vs), Vane tip (coef_vtip)
    
    common/operational_para/rev, omega_1, alpha_1, coef_sucport, coef_discport, rho_dv, E_dv, dpratio_dv, freq, omega_b, rev_b, T_0, p_0, coef_vef, coef_ref, coef_rad, T_oil_sump, T_oil_flow 
    common/valve_para_2/coef_dv, coef_dv_back
    common/valve_para_3/sur_tension_oil, angle_oil_film, ratio_val_port
    common/fric_coef/coef_vs, coef_vhs, coef_circ_lip
    
    ! include "var_operational_parameter.f90"