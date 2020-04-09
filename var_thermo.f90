
    double precision, dimension (1:no_data+1) :: p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv
    double precision, dimension (1:no_data+1) :: p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv
    double precision, dimension (1:no_data+1) :: dmdtheta1_si, dmdtheta1_so, dmdtheta1_s, dedtheta1_s, dqdtheta1_s
    double precision, dimension (1:no_data+1) :: dmdtheta1_di, dmdtheta1_do, dmdtheta1_d, dedtheta1_d, dqdtheta1_d
    double precision mass_residual_dead, h_residual_dead        ! residual mass caused by dead volume
    
    common/reexpand/mass_residual_dead, h_residual_dead
    ! include "var_thermo.f90"