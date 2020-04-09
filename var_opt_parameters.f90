    !integer obj_func_input, obj_var_set
    !common/optimization_obj/obj_func_input, obj_var_set
    !integer opt_ori_complex, opt_i, opt_re
    !common/optimization_para/opt_ori_complex, opt_i
    !double precision vol_min, vol_max, vol_max_2                                   ! Minimum and Maximum volume, and maximum volume for set 2
    !common/optimization_cond/vol_min, vol_max, vol_max_2
    
    double precision factor_a, factor_b     ! reflection factor "alpha", and constraints boundary factor "beta"
    common/optimization_para_re/factor_a, factor_b  
    integer obj_func_input, obj_var_set
    common/optimization_obj/obj_func_input, obj_var_set
    integer opt_ori_complex, opt_i, opt_re
    common/optimization_para/opt_ori_complex, opt_i, opt_re
    double precision vol_min, vol_max, vol_max_2                                  ! Minimum and Maximum volume, and maximum volume for set 2
    common/optimization_cond/vol_min, vol_max, vol_max_2
    
    
    ! include "var_opt_parameters.f90" 