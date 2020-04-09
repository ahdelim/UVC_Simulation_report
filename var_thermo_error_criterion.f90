    
    ! include "var_thermo_error_criterion.f90"
    
    double precision error_pos, error_poc, error_tos, error_toc
    double precision error_pis, error_pic, error_tis, error_tic
    double precision conv_criterion, error_default
    common/thermo_error_crit/error_pos, error_poc, error_tos, error_toc, error_pis, error_pic, error_tis, error_tic
    common/conv_cri/conv_criterion, error_default
    ! p=pressure, o=outer, s=suction, c = compression, t=temperature, r=rho=density, i=inner