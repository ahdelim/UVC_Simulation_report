double precision t_low, t_high                  ! Lowest temperature & Highest Temperature (according to ASHRAE/ASRI standard)
double precision t_disc, p_disc, rho_disc, h_disc, s_disc, u_disc, miu_disc, cv_disc, cp_disc, k_disc            ! Discharge condition
double precision t_suc, p_suc, rho_suc, h_suc, s_suc, u_suc, miu_suc, cv_suc, cp_suc, k_suc            ! Suction condition
double precision p_disc_air, t_disc_air, p_suc_air, t_suc_air

common/suction_cond/t_suc, p_suc, rho_suc, h_suc, s_suc, u_suc, miu_suc, cv_suc, cp_suc, k_suc, t_low      
common/discharge_cond/t_disc, p_disc, rho_disc, h_disc, s_disc, u_disc, miu_disc, cv_disc, cp_disc, k_disc, t_high


!include "var_operating_fluid_condition.f90"