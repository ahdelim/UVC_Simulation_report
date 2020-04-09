subroutine discharge_valve_parameters(no_data, A_cross_dv_1, I_dv_1, beta_r_1)
    
    implicit none
    include "var_main_dimensions.f90"
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    integer no_data, i
    double precision A_cross_dv_1, I_dv_1, beta_r_1, omega_natural_dv_rec, omega_natural_dv_back_rec         ! Natural frequency of rectangular valve
    common/rec_valve_nat_freq/omega_natural_dv_rec, omega_natural_dv_back_rec
    
    A_cross_dv_1 = w_dv_rec * t_dv * 1.d-6
    I_dv_1 = ((w_dv_rec*1.d-3) * (t_dv*1.d-3)**3 )/12.0
    beta_r_1 = 1.87510407/(l_dv_rec_ef*1.d-3)
    omega_natural_dv_rec = sqrt((E_dv*I_dv_1*beta_r_1**4)/(rho_dv*A_cross_dv_1))
    !omega_response_dv_rec = sqrt(omega_natural_dv_rec**2)
    omega_natural_dv_back_rec = 5428.41661299 !4652.92853059   ! rad/s, found from K.R. Heng FEM model
    
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' Valve parameters (Rectangular Valve) '
    print *, ' ---------------------------------------------------------------------------- '
    print '(2x,A,F15.4,A)', 'Natural Frequency of rectangular valve              = ', omega_natural_dv_rec, ' rad/s'
    print '(2x,A,F15.4,A)', 'Natural Frequency of rectangular valve (stop plate) = ', omega_natural_dv_back_rec, ' rad/s'
    print *, ' ---------------------------------------------------------------------------- '

    
endsubroutine discharge_valve_parameters