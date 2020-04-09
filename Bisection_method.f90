subroutine bisection_method(switch_case, lhs_eq, k_ratio, mach_a, mach_b, mach_final)
    implicit none
    double precision mach_a, mach_b, mach_c, f_mach_a, f_mach_b, f_mach_c   ! input mach_a, mach_b ... dummy mach_c, function of f_mach_a, f_mach_b, f_mach_c
    double precision lhs_eq, k_ratio    ! input, left hand side euqation and k_ratio
    double precision mach_final     ! final mach
    integer switch_case     ! to switch equation, 1 for fanno flow (radial clearance)
    
103         continue
        
    if (switch_case == 1) then
        call leakage_channel(mach_a, k_ratio, lhs_eq, f_mach_a)
        call leakage_channel(mach_b, k_ratio, lhs_eq, f_mach_b)
    endif
    
    if (f_mach_a*f_mach_b .lt. 0.0) then
        mach_c = (mach_a + mach_b)/2.0      ! root found between range a and b, so that f(a)*f(b) is negative (f(root) = 0)
    else
        goto 102
    endif
            
    if (switch_case == 1) then
        call leakage_channel(mach_c, k_ratio, lhs_eq, f_mach_c)        ! to calculate the friction channel length
    endif
    
    if (f_mach_a*f_mach_c .lt. 0.0) then        ! Root found between range a and c, narrower down the range, so replace b
        mach_b = mach_c         
    else
        mach_a = mach_c
    endif

    if (abs((mach_b - mach_a)) .ge. 1.d-9) then !/mach_e_b   ! 1.d-8        ! Check for convergence
        goto 103
    else
        mach_final = mach_c       ! record down the final answer (which result in error +- 1%)
        goto 102
    endif
            
102         continue
    
endsubroutine bisection_method