subroutine leakage_fanno(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, length_flow, error_press, leak_conv_criterion, press_ratio, mach_t, clearance, l_fanno_eq, mach_e, dmdtheta_leak)
    ! ----------------------------
    ! Fanno flow through a convergent duct
    ! ----------------------------
    implicit none
    ! ----- input ------ !
    double precision T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, length_flow
    double precision error_press, leak_conv_criterion, press_ratio, mach_t, clearance, lambda_factor, lambda_factor_b, l_f_assumed, l_fanno_eq
    double precision dummy_A, dummy_B, dummy_C, dummy_D
    double precision mach_e, dmdtheta_leak      ! --- output ---
    double precision mach_e_a, mach_e_b, mach_e_c, f_mach_e_a, f_mach_e_b, f_mach_e_c, pt_pstar, pd_pt, pe_pstar, pd_pe
    double precision mach_t_a, mach_t_b, mach_t_max, mach_t_factor
    integer m, switch_case
    m = 1
    switch_case = 1
    mach_t_factor = 0.00001
    ! ------ calculate lambda --------
    ! Assuming choked flow (exit mach number is 1)
    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
    
    ! ---- Equivalent friction channel length (l_f that makes exit flow is critical, which Mach number = 1)
    dummy_D = lambda_factor*(l_fanno_eq)/(2.0*clearance)      ! dummy variables (equation at left hand side)
    
    mach_t_a = 0.000000001
    mach_t_b = 0.999999999
    
    call bisection_method(switch_case, dummy_D, k_ratio, mach_t_a, mach_t_b, mach_t_max)        ! assume exit is choked (mach_e = 1.0), find mach_t for the root
    ! mach_t_max meaning the mach_t that cause exit flow choked, so it is maximum mach number at throat

    pt_pstar = 1.0/mach_t_max*sqrt((k_ratio + 1.0)/(2.0 + mach_t_max**2*(k_ratio - 1.0)))
    pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t_max**2)**(k_ratio/(k_ratio - 1.0))
    pd_pe = pd_pt*pt_pstar
    
    ! ---------------------------------
    ! If compressor current pressure ratio > calculated pressure ratio when the flow is choked, 
    ! then it is Choked Flow
    ! ---------------------------------
    if (pd_pe > press_ratio) then
        mach_t = mach_t_max+0.01
        goto 1055   ! meaning not choked
    else
        mach_e = 1.0
        goto 1056   ! choked flow, skip the iteration.
    endif
1055 continue    
    !call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
    do while (abs(error_press) > leak_conv_criterion)
            ! --- Dummy variables for ease of calculation to solve for l_f_assumed
            dummy_A = (1.0 - mach_t**2)/(k_ratio*mach_t**2)
            dummy_B = (k_ratio + 1.0)/(2.*k_ratio)
            dummy_C = (k_ratio + 1.0)*mach_t**2/(2.0 + (k_ratio - 1.0)*mach_t**2)
            ! ---- Equivalent friction channel length (l_f that makes exit flow is critical, which Mach number = 1)
            l_f_assumed = ((2.0*clearance)/lambda_factor)*(dummy_A + dummy_B*log(dummy_C))
            dummy_D = lambda_factor*(l_f_assumed - l_fanno_eq)/(2.0*clearance)      ! dummy variables (lhs equation)
            
            ! ----------------------------
            ! Check Pressure Ratio
            ! ----------------------------
            !pt_pe = 1.0/mach_t*sqrt((k_ratio + 1.0)/(2.0 + mach_t**2*(k_ratio - 1.0)))
            !pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
            !pd_pe = pd_pt*pt_pe
            !error_press = abs(pd_pe - press_ratio)/press_ratio
            !if (error_press < leak_conv_criterion) then
            !    goto 102
            !endif
            
            ! -------------------------------------------
            ! Bisection method to find root of mach_e
            ! equation is match when the error is between +- 1%
            ! leakage_channel is the subroutine to calculate f_mach_e_a and f_mach_e_b (part of Bisection Method)
            ! based on guessed value mach_e_a and mach_e_b.
            ! by comparing result of a and b, the mid value is taken and calcualte again for compare
            ! -------------------------------------------
            !mach_e_a = 0.0000000001     
            !mach_e_b = 0.9999999999
            mach_e_a = mach_t
            mach_e_b = 0.999999
            call bisection_method(switch_case, dummy_D, k_ratio, mach_e_a, mach_e_b, mach_e)
            
            pt_pstar = 1.0/mach_t*sqrt((k_ratio + 1.0)/(2.0 + mach_t**2*(k_ratio - 1.0)))
            pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
            pe_pstar = 1.0/mach_e*sqrt((k_ratio + 1.0)/(2.0 + mach_e**2*(k_ratio - 1.0)))
            pd_pe = pd_pt*pt_pstar/pe_pstar

            error_press = (pd_pe - press_ratio)/press_ratio
            
            !pause
            if (pd_pe > press_ratio .and. abs(error_press) > leak_conv_criterion) then
                !mach_t = mach_t + 0.0000001         
                !if (m < 5000) then
                !if (press_ratio < 2) then
                    !print'(1x,5F10.4)', mach_t, mach_e, error_press, press_ratio
                    mach_t = mach_t - abs(error_press)*0.001 !abs(error_press)*0.001
                    !mach_t = mach_t - 0.0001
                    if (mach_t > (mach_t_max + 0.01) .or. mach_t < 0.0 .or. mach_t > 1.0) then
                        !mach_t = 0.00000001
                        !mach_t = 0.2999
                        mach_t = mach_t_max+0.01 - 0.00001!abs(error_press)*0.001
                        !mach_t = mach_t_max - error_press*0.001
                        !mach_t = 0.0 + abs(error_press)*0.001
                    endif
                !endif
                m = m + 1
            elseif (pd_pe < press_ratio .and. abs(error_press) > leak_conv_criterion) then
                    mach_t = mach_t - abs(error_press)*0.001
                    if (mach_t > mach_t_max+0.01 .or. mach_t < 0.0 .or. mach_t > 1.0) then
                        !mach_t = 0.00000001
                        !mach_t = 0.2999
                        mach_t = mach_t_max+0.01 - abs(error_press)*0.0001
                        !mach_t = 0.0 + abs(error_press)*0.000001
                    endif
                    m = m + 1
            endif
            !elseif (pd_pe < press_ratio .and. abs(error_press) <= leak_conv_criterion) then
            !    mach_t = mach_t_max
            !    mach_e = mach_unity
            !    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_e, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
            !    goto 1056
            
            !endif
    !call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_e, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor_b, dmdtheta_leak)
    !lambda_factor = 0.5*(lambda_factor_b + lambda_factor)
    ! ----- end here --------
            !write(6,2982) press_ratio, pt_pe, mach_t, mach_e, error_press   ! for checking in-loop, on screen
        if (m > 12000) then
            !print*, m,leak_conv_criterion
            leak_conv_criterion = 2.0 * leak_conv_criterion
            m = 1
        endif
    enddo
    !write(99,2982) press_ratio, pd_pe, mach_t, mach_e, error_press  ! for checking
    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_e, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
1056    continue
return
2982 format (7F8.4) 
endsubroutine leakage_fanno




!103         continue
!            call leakage_channel(mach_e_a, k_ratio, dummy_D, f_mach_e_a)
!            call leakage_channel(mach_e_b, k_ratio, dummy_D, f_mach_e_b)
!            
!            if (f_mach_e_a*f_mach_e_b .lt. 0.0) then
!                mach_e_c = (mach_e_a + mach_e_b)/2.0
!            else
!                goto 102
!            endif
!            
!            call leakage_channel(mach_e_c, k_ratio, dummy_D, f_mach_e_c)        ! to calculate the friction channel length
!                
!            if (f_mach_e_a*f_mach_e_c .lt. 0.0) then
!                mach_e_b = mach_e_c
!            else
!                mach_e_a = mach_e_c
!            endif
!
!            if (abs((mach_e_b - mach_e_a)) .ge. 0.000001) then !/mach_e_b
!                goto 103
!            else
!                mach_e = mach_e_c       ! record down the final answer (which result in error +- 1%)
!                goto 102
!            endif
!            
!102         continue