! include "var_simulation_parameter.f90"
    
    double precision theta_step, theta_end, theta_start, theta_step_low, theta_step_high, theta_step_degree_1, theta_step_degree_low, theta_step_degree_high       ! step_size, theta_end, theta_start
    integer no_data, no_data_low, no_data_high       ! Number of Data points
    !integer data_step, data_step_suct, data_step_high, max_write_data
    real data_step, data_step_suct, data_step_high, max_write_data
    common/simulation_para/theta_step, theta_step_low, theta_step_high, theta_end, theta_start, theta_step_degree_1, theta_step_degree_low, theta_step_degree_high
    common/simulation_para_2/no_data, no_data_low, no_data_high, data_step, data_step_suct, data_step_high, max_write_data