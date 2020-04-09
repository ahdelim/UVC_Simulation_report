subroutine discharge_valve_parameters_ver2(w_dv_x3, t_dv, l_dvef, w_dv_x2, r_dv_x1, l_dv_x2, l_dv_x3, omega_natural_dv, omega_natural_dv_back)
    ! --------------------------------------------------------
    ! To find natural frequency using
    ! Rayleigh-quotient
    ! --------------------------------------------------------
    implicit none
    !include "var_main_dimensions.f90"
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    integer n, i, j
    !double precision, dimension (1:no_data+1) :: omega_response_dv_1      ! response frequency
    
    double precision omega_natural_dv, omega_natural_dv_back         ! Natural frequency
    double precision A_cross_x1, A_cross_x2, A_cross_x3, I_x1, I_x2, I_x3         ! Valve parameters
    double precision w_dv_x3, t_dv, l_dvef, w_dv_x2, r_dv_x1, l_dv_x2, l_dv_x3
    double precision h_x1, h_x2, h_x3, x1,x2,x3, y_up_x1, y_up_x2, y_up_x3, y_down_x1, y_down_x2, y_down_x3         ! For weddle rule estimation (dummy varaibles)
    double precision f_y_up_x1, f_y_up_x2, f_y_up_x3, f_y_down_x1, f_y_down_x2, f_y_down_x3                         ! For weddle rule estimation
    double precision dummy_A_cross_x1
    
    double precision y_up_x1_back, y_up_x2_back, y_up_x3_back, y_down_x1_back, y_down_x2_back, y_down_x3_back
    double precision f_y_up_x1_back, f_y_up_x2_back, f_y_up_x3_back, f_y_down_x1_back, f_y_down_x2_back, f_y_down_x3_back
    integer optimization_opt
    common/routine_opt/optimization_opt
    integer opt_i
    common/optimization_para/opt_i
    
    !A_cross_dv_1 = w_dv_1 * t_dv_1 * 1.d-6
    !I_dv_1 = ((w_dv_1*1.d-3) * (t_dv_1*1.d-3)**3 )/12.0
    !beta_r_1 = 1.87510407/(l_dv_1*1.d-3)
    !omega_natural_dv_1 = sqrt((E_dv*I_dv_1*beta_r_1**4)/(rho_dv*A_cross_dv_1))
    
    A_cross_x2 = w_dv_x2 * t_dv * 1.d-6
    !A_cross_x3 = w_dv_x3 * t_dv * 1.d-6
    I_x2 = (1./12.0) * (w_dv_x2*1.d-3) * (t_dv*1.d-3)**3
    !I_x3 = (1./12.0) * (w_dv_x3*1.d-3) * (t_dv*1.d-3)**3 ! w/o part 3
    
    ! ----------------------------
    ! Initialize dummy variables
    ! 1 = start of the port to the neck
    ! 2 = the neck
    ! 3 = the body to the screw
    ! ----------------------------
    !h_x1 = 2*r_dv_x1*1.d-3/6.       ! step increment of length of each part (x1)
    h_x1 = ((2*r_dv_x1 - 0.77)*1.d-3)/6.       ! step increment of length of each part (x1), corrected
    h_x2 = ((l_dv_x3 - l_dv_x2)*1.d-3)/6.
    !h_x3 = ((l_dvef - l_dv_x3)*1.d-3)/6. ! w/o part 3
    x1 = 0.0     ! start point of length of part x1..
    x2 = l_dv_x2*1.d-3
    !x3 = l_dv_x3*1.d-3 ! w/o part 3
    y_up_x1 = 0.        ! initialization
    y_up_x2 = 0.
    !y_up_x3 = 0. ! w/o part 3
    y_down_x1 = 0.
    y_down_x2 = 0.
    !y_down_x3 = 0. ! w/o part 3
    y_up_x1_back = 0.
    y_up_x2_back = 0.
    !y_up_x3_back = 0. ! w/o part 3
    y_down_x1_back = 0.
    y_down_x2_back = 0.
    !y_down_x3_back = 0. ! w/o part 3
    
    ! --------------------------------
    ! Integration with Weddle's Rule
    ! --------------------------------
    !do j = 1,216
        do 370 i = 1,7
            dummy_A_cross_x1 = (2.*(r_dv_x1*1.d-3)*x1 - x1**2)
            if (dummy_A_cross_x1 .le. 0.0000) then
                A_cross_x1 = 0.000000
            endif
            A_cross_x1 = 2.*(t_dv*1.d-3)*dummy_A_cross_x1**(0.5)
            I_x1 = (((t_dv*1.d-3)**3)/6.)*dummy_A_cross_x1**(0.5)
            
            ! -----------------------------------------------------
            ! Case - when hit the back plate
            ! Coefficients are unified to have dimension in "metre"
            ! Calculating 'X' in the 1992, paper by K.T. Ooi
            ! f_y_up = E*I*(d2phi/dx2)^2
            ! f_y_down = rho*A*phi^2
            ! due to unit problem (m and mm), correction scale have to be added (1.d-3)
            ! -----------------------------------------------------
            !f_y_up_x1_back = E_dv*I_x1*((12.*x1**2*(1.d-3)**2/(l_dvef*1.d-3)**4 - 9*x1*(1.d-3)**2/(l_dvef*1.d-3)**3)*1.d-3)**2
            !f_y_up_x2_back = E_dv*I_x2*((12.*x2**2*(1.d-3)**2/(l_dvef*1.d-3)**4 - 9*x2*(1.d-3)**2/(l_dvef*1.d-3)**3)*1.d-3)**2
            !f_y_up_x3_back = E_dv*I_x3*((12.*x3**2*(1.d-3)**2/(l_dvef*1.d-3)**4 - 9*x3*(1.d-3)**2/(l_dvef*1.d-3)**3)*1.d-3)**2
            !
            !f_y_down_x1_back = rho_dv*A_cross_x1*((1*(x1/(l_dvef*1.d-3))**4 - (1.5)*(x1/(l_dvef*1.d-3))**3 + (0.5)*(x1/(l_dvef*1.d-3)))*1.d-3)**2
            !f_y_down_x2_back = rho_dv*A_cross_x2*((1*(x2/(l_dvef*1.d-3))**4 - (1.5)*(x2/(l_dvef*1.d-3))**3 + (0.5)*(x2/(l_dvef*1.d-3)))*1.d-3)**2
            !f_y_down_x3_back = rho_dv*A_cross_x3*((1*(x3/(l_dvef*1.d-3))**4 - (1.5)*(x3/(l_dvef*1.d-3))**3 + (0.5)*(x3/(l_dvef*1.d-3)))*1.d-3)**2
            ! -----------------------------------
            ! unified to mm
            ! However, no unit for mode shape
            ! -----------------------------------
            f_y_up_x1_back = E_dv*I_x1*((12.*x1**2/(l_dvef*1.d-3)**4 - 9*x1/(l_dvef*1.d-3)**3))**2
            f_y_up_x2_back = E_dv*I_x2*((12.*x2**2/(l_dvef*1.d-3)**4 - 9*x2/(l_dvef*1.d-3)**3))**2
            !f_y_up_x3_back = E_dv*I_x3*((12.*x3**2/(l_dvef*1.d-3)**4 - 9*x3/(l_dvef*1.d-3)**3))**2     ! w/o part 3
        
            f_y_down_x1_back = rho_dv*A_cross_x1*((1*(x1/(l_dvef*1.d-3))**4 - (1.5)*(x1/(l_dvef*1.d-3))**3 + (0.5)*(x1/(l_dvef*1.d-3))))**2
            f_y_down_x2_back = rho_dv*A_cross_x2*((1*(x2/(l_dvef*1.d-3))**4 - (1.5)*(x2/(l_dvef*1.d-3))**3 + (0.5)*(x2/(l_dvef*1.d-3))))**2
            !f_y_down_x3_back = rho_dv*A_cross_x3*((1*(x3/(l_dvef*1.d-3))**4 - (1.5)*(x3/(l_dvef*1.d-3))**3 + (0.5)*(x3/(l_dvef*1.d-3))))**2    ! w/o part 3
        
            ! ---------------------------------------
            ! Case - after Left seat only (before hitting back plate)
            ! Case 1 = left seat
            ! Case 2 = hit the back plate
            ! both having different mode shape
            ! ---------------------------------------
            !f_y_up_x1 = E_dv*I_x1*((12.*((x1**2)*(1.d-3)**2/((l_dvef*1.d-3)**4)))*1.d-3)**2
            !f_y_up_x2 = E_dv*I_x2*((12.*((x2**2)*(1.d-3)**2/((l_dvef*1.d-3)**4)))*1.d-3)**2
            !f_y_up_x3 = E_dv*I_x3*((12.*((x3**2)*(1.d-3)**2/((l_dvef*1.d-3)**4)))*1.d-3)**2
            !
            !f_y_down_x1 = rho_dv*A_cross_x1*((1*(x1/(l_dvef*1.d-3))**4 - 4.0*(x1/(l_dvef*1.d-3)) + 3.0)*1.d-3)**2
            !f_y_down_x2 = rho_dv*A_cross_x2*((1*(x2/(l_dvef*1.d-3))**4 - 4.0*(x2/(l_dvef*1.d-3)) + 3.0)*1.d-3)**2
            !f_y_down_x3 = rho_dv*A_cross_x3*((1*(x3/(l_dvef*1.d-3))**4 - 4.0*(x3/(l_dvef*1.d-3)) + 3.0)*1.d-3)**2
            ! -----------------------------------
            ! unified to mm
            ! -----------------------------------
            f_y_up_x1 = E_dv*I_x1*((12.*((x1**2)/((l_dvef*1.d-3)**4))))**2
            f_y_up_x2 = E_dv*I_x2*((12.*((x2**2)/((l_dvef*1.d-3)**4))))**2
            !f_y_up_x3 = E_dv*I_x3*((12.*((x3**2)/((l_dvef*1.d-3)**4))))**2 ! w/o part 3
        
            f_y_down_x1 = rho_dv*A_cross_x1*((1*(x1/(l_dvef*1.d-3))**4 - 4.0*(x1/(l_dvef*1.d-3)) + 3.0))**2
            f_y_down_x2 = rho_dv*A_cross_x2*((1*(x2/(l_dvef*1.d-3))**4 - 4.0*(x2/(l_dvef*1.d-3)) + 3.0))**2
            !f_y_down_x3 = rho_dv*A_cross_x3*((1*(x3/(l_dvef*1.d-3))**4 - 4.0*(x3/(l_dvef*1.d-3)) + 3.0))**2 ! w/o part 3
        
            if ( i == 1 .or. i == 3 .or. i == 5 .or. i == 7) then
                n = 1
            elseif (i == 2 .or. i == 6) then
                n = 5
            elseif (i == 4) then
                n = 6
            endif
        
            ! weddle rules summation
            y_up_x1 = y_up_x1 + 0.3*h_x1*n*f_y_up_x1
            y_up_x2 = y_up_x2 + 0.3*h_x2*n*f_y_up_x2
            !y_up_x3 = y_up_x3 + 0.3*h_x3*n*f_y_up_x3 ! w/o part 3
        
            y_down_x1 = y_down_x1 + 0.3*h_x1*n*f_y_down_x1
            y_down_x2 = y_down_x2 + 0.3*h_x2*n*f_y_down_x2
            !y_down_x3 = y_down_x3 + 0.3*h_x3*n*f_y_down_x3 ! w/o part 3
        
            y_up_x1_back = y_up_x1_back + 0.3*h_x1*n*f_y_up_x1_back
            y_up_x2_back = y_up_x2_back + 0.3*h_x2*n*f_y_up_x2_back
            !y_up_x3_back = y_up_x3_back + 0.3*h_x3*n*f_y_up_x3_back ! w/o part 3
        
            y_down_x1_back = y_down_x1_back + 0.3*h_x1*n*f_y_down_x1_back
            y_down_x2_back = y_down_x2_back + 0.3*h_x2*n*f_y_down_x2_back
            !y_down_x3_back = y_down_x3_back + 0.3*h_x3*n*f_y_down_x3_back ! w/o part 3
            !print*, A_cross_x1, dummy_A_cross_x1, x2
            x1 = x1 + h_x1
            x2 = x2 + h_x2
            !x3 = x3 + h_x3
370     continue    
    !enddo
        ! -------------------------------------------------------------
        ! Natural Frequency of normal case and when hit the back plate
        ! -------------------------------------------------------------
        !omega_natural_dv = sqrt((y_up_x1 + y_up_x2 + y_up_x3)/(y_down_x1 + y_down_x2 + y_down_x3))    ! Rayleigh quotient   
        omega_natural_dv = sqrt((y_up_x1 + y_up_x2)/(y_down_x1 + y_down_x2))    ! Rayleigh quotient, without part 3
        !omega_natural_dv_back = sqrt((y_up_x1_back + y_up_x2_back + y_up_x3_back)/(y_down_x1_back + y_down_x2_back + y_down_x3_back))
        omega_natural_dv_back = sqrt((y_up_x1_back + y_up_x2_back)/(y_down_x1_back + y_down_x2_back)) ! without part 3
        print *, ' ---------------------------------------------------------------------------- '
        print *, ' Valve parameters '
        print *, ' ---------------------------------------------------------------------------- '
        print '(2x,A,F15.4,A)', 'Natural Frequency of valve                         = ', omega_natural_dv, ' rad/s'
        print '(2x,A,F15.4,A)', 'Natural Frequency of valve when hit the stop plate = ', omega_natural_dv_back, ' rad/s'
        print *, ' ---------------------------------------------------------------------------- '
        print *, ' '
        print *, ' '
        print *, ' '
        print *, ' '
        if (opt_i == 0) then
            write (113,*) " -------------------------------------------------------------------- "
            write (113,*) " Valve Vibration Parameters "
            write (113,*) " -------------------------------------------------------------------- "
            write (113,2989) 'Natural Frequency of valve                         = ', omega_natural_dv, ' rad/s'
            write (113,2989) 'Natural Frequency of valve when hit the stop plate = ', omega_natural_dv_back, ' rad/s'
            write (113,*) ' '
        endif
        
            
2989 format (2x,A,F15.4,A)

     
endsubroutine discharge_valve_parameters_ver2