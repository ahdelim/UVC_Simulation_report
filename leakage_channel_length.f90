subroutine leakage_channel(M, k, left_hand_eq, f_mach_e)
    ! -------------------------------------------
    ! Bisection method to find root of mach_e
    ! equation is match when the error is between +- 1%
    ! leakage_channel is the subroutine to calculate f_mach_e_a and f_mach_e_b (part of Bisection Method)
    ! based on guessed value mach_e_a and mach_e_b
    ! by comparing result of a and b, the mid value is taken and calcualte again for compare
    ! -------------------------------------------
    implicit none
    double precision M,k,left_hand_eq      ! Mach number, k (specific heat ratio), clearance, channel_length*lambda (left hand side of the equation)
    double precision dummy_A_e, dummy_B_e, dummy_C_e, dummy_D_e
    double precision f_mach_e
    
    
    dummy_A_e = (1.0 - M**2)/(k*M**2)
    dummy_B_e = (k + 1.0)/(2.*k)
    dummy_C_e = ((k + 1.0)*M**2)/(2.0 + (k - 1.0)*M**2)
    dummy_D_e = (dummy_A_e + dummy_B_e*log(dummy_C_e))
    f_mach_e = dummy_D_e - left_hand_eq
    
endsubroutine leakage_channel
    
    
    