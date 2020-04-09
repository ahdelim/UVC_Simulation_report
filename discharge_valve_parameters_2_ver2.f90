!subroutine discharge_valve_parameters_2_ver2(no_data, A_cross_dv_1, I_dv_1, beta_r_1, omega_natural_dv_1, omega_response_dv_1)
!    
!    implicit none
!    include "var_main_dimensions.f90"
!    include "var_operational_parameter.f90"
!    include "var_physical_constants.f90"
!    integer no_data, n, i
!    double precision, dimension (1:no_data+1) :: omega_response_dv_1      ! response frequency
!    double precision A_cross_dv_1, I_dv_1, beta_r_1, omega_natural_dv_1         ! Natural frequency
!    double precision A_cross_dv1_x1, A_cross_dv1_x2, I_dv1_x1, I_dv1_x2
!    double precision h_x1, h_x2, h_x3, x1,x2,x3, y_up_x1, y_up_x2, y_up_x3, y_down_x1, y_down_x2, y_down_x3
!    double precision f_y_up_x1, f_y_up_x2, f_y_up_x3, f_y_down_x1, f_y_down_x2, f_y_down_x3
!    
!    !A_cross_dv_1 = w_dv_1 * t_dv_1 * 1.e-6
!    !I_dv_1 = ((w_dv_1*1.e-3) * (t_dv_1*1.e-3)**3 )/12.0
!    !beta_r_1 = 1.87510407/(l_dv_1*1.e-3)
!    !omega_natural_dv_1 = sqrt((E_dv*I_dv_1*beta_r_1**4)/(rho_dv*A_cross_dv_1))
!    
!    A_cross_dv1_x1 = w_dv_1 * t_dv_1 * 1.e-6
!    A_cross_dv1_x2 = w_dv1_x2 * t_dv_1 * 1.e-6
!    I_dv1_x1 = (1./12.0) * (w_dv_1*1.e-3) * (t_dv_1*1.e-3)**3
!    I_dv1_x2 = (1./12.0) * (w_dv1_x2*1.e-3) * (t_dv_1*1.e-3)**3
!    
!    h_x1 = l_dv1ef_x1*1.e-3 / 6.
!    h_x2 = (x_hole_start_1 - l_dv1ef_x1)*1.e-3/6.
!    h_x3 = (l_dv1ef - x_hole_start_1)*1.e-3/6.
!    x1 = 0.
!    x2 = l_dv1ef_x1*1.e-3
!    x3 = x_hole_start_1*1.e-3
!    y_up_x1 = 0.
!    y_up_x2 = 0.
!    y_up_x3 = 0.
!    y_down_x1 = 0.
!    y_down_x2 = 0.
!    y_down_x3 = 0.
!    
!    do 371 i = 1,7
!        f_y_up_x1 = E_dv*I_dv1_x1*(12.*x1**2/(l_dv1ef*1.e-3)**4 - 9*x1/(l_dv1ef*1.e-3)**3)**2
!        f_y_up_x2 = E_dv*I_dv1_x2*(12.*x2**2/(l_dv1ef*1.e-3)**4 - 9*x2/(l_dv1ef*1.e-3)**3)**2
!        f_y_up_x3 = E_dv*(((t_dv_1*1.e-3)**3)/6.)*(2*(r_dv1_x3*1.e-3)*(x3 - x_hole_start_1*1.e-3) - (x3 - x_hole_start_1*1.e-3)**2)**(0.5) * (12.*x3**2/(l_dv1ef*1.e-3)**4 - 9*x3/(l_dv1ef*1.e-3)**3)**2
!        f_y_down_x1 = rho_dv*A_cross_dv1_x1*((x1/(l_dv1ef*1.e-3))**4 - (1.5)*(x1/(l_dv1ef*1.e-3))**3 + (0.5)*(x1/(l_dv1ef*1.e-3)))**2
!        f_y_down_x2 = rho_dv*A_cross_dv1_x2*((x2/(l_dv1ef*1.e-3))**4 - (1.5)*(x2/(l_dv1ef*1.e-3))**3 + (0.5)*(x2/(l_dv1ef*1.e-3)))**2
!        f_y_down_x3 = rho_dv*2.*(t_dv_1*1.e-3)*(2.*(r_dv1_x3*1.e-3)*(x3 - x_hole_start_1*1.e-3) - (x3 - x_hole_start_1*1.e-3)**2)**(0.5) * ((x2/(l_dv1ef*1.e-3))**4 - (1.5)*(x2/(l_dv1ef*1.e-3))**3 + (0.5)*(x2/(l_dv1ef*1.e-3)))**2
!        
!        if ( i == 1 .or. i == 3 .or. i == 5 .or. i == 7) then
!            n = 1
!        elseif (i == 2 .or. i == 6) then
!            n = 5
!        elseif (i == 4) then
!            n = 6
!        endif
!        
!        y_up_x1 = y_up_x1 + 0.3*h_x1*n*f_y_up_x1
!        y_up_x2 = y_up_x2 + 0.3*h_x2*n*f_y_up_x2
!        y_up_x3 = y_up_x3 + 0.3*h_x3*n*f_y_up_x3
!        y_down_x1 = y_down_x1 + 0.3*h_x1*n*f_y_down_x1
!        y_down_x2 = y_down_x2 + 0.3*h_x2*n*f_y_down_x2
!        y_down_x3 = y_down_x3 + 0.3*h_x3*n*f_y_down_x3
!        
!        x1 = x1 + h_x1
!        x2 = x2 + h_x2
!        x3 = x3 + h_x3
!        
!371 continue    
!    print *, y_up_x1 + y_up_x2 + y_up_x3, y_down_x1 + y_down_x2 + y_down_x3
!    do 372 i=1,no_data+1
!        
!        !omega_response_dv_1(i) = sqrt(omega_natural_dv_1**2)        ! Fixed, no centrifugal effect
!        omega_response_dv_1(i) = sqrt((y_up_x1 + y_up_x2 + y_up_x3)/(y_down_x1 + y_down_x2 + y_down_x3)) 
!        
!372 continue        
!    
!endsubroutine discharge_valve_parameters_2_ver2