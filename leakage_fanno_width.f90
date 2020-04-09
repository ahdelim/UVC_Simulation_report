subroutine leakage_fanno_width(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, length_flow, error_press, leak_conv_criterion, press_ratio, mach_t, clearance, l_fanno_eq, mach_e, dmdtheta_leak)
    ! ----------------------------
    ! Fanno flow without convergent duct
    ! ----------------------------
    implicit none
    ! ----- input ------ !
    double precision T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, length_flow
    double precision error_press, leak_conv_criterion, press_ratio, mach_t, clearance, lambda_factor, l_f_assumed, l_fanno_eq
    double precision dummy_A, dummy_B, dummy_C, dummy_D
    double precision mach_e, dmdtheta_leak      ! --- output ---
    double precision mach_e_a, mach_e_b, mach_e_c, f_mach_e_a, f_mach_e_b, f_mach_e_c, pt_pstar, pt_pe, pe_pstar
    double precision mach_t_a, mach_t_b, mach_t_max, mach_t_factor
    integer m, switch_case
    m = 1
    switch_case = 1
    mach_t_factor = 0.00001
    ! Assuming choked flow (exit mach number is 1)
    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
    
    ! ---- Equivalent friction channel length (l_f that makes exit flow is critical, which Mach number = 1)
    dummy_D = lambda_factor*(l_fanno_eq)/(2.0*clearance)      ! dummy variables (equation left)
    
    mach_t_a = 0.00000001     
    mach_t_b = 0.99999999
    call bisection_method(switch_case, dummy_D, k_ratio, mach_t_a, mach_t_b, mach_t_max)
    
    pt_pstar = 1.0/mach_t_max*sqrt((k_ratio + 1.0)/(2.0 + mach_t_max**2*(k_ratio - 1.0)))
    !pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
    !pe_pstar = 1.0/mach_unity*sqrt((k_ratio + 1.0)/(2.0 + mach_unity**2*(k_ratio - 1.0)))      ! because pe/pstar is 1.0 when the flow is choked! pstar is the pressure if the flow is choked
    pt_pe = pt_pstar        ! pe = p_star in this case, because the exit mach number is 1.0
    
    ! ---------------------------------
    ! If compressor current pressure ratio > calculated pressure ratio when the flow is choked, 
    ! then it is Choked Flow
    ! ---------------------------------
    !if (abs((pt_pe - press_ratio)/press_ratio) < 0.03 .and. pt_pe < press_ratio) then
    if (pt_pe < press_ratio) then
        mach_e = 1.0
        goto 1056   ! choked flow
    else
        mach_t = mach_t_max+0.1
        goto 1055   ! meaning not choked
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
            dummy_D = lambda_factor*(l_f_assumed - l_fanno_eq)/(2.0*clearance)      ! dummy variables (equation left)

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
            mach_e_a = 1.0 
            mach_e_b = mach_t
            call bisection_method(switch_case, dummy_D, k_ratio, mach_e_a, mach_e_b, mach_e)

            pt_pstar = 1.0/mach_t*sqrt((k_ratio + 1.0)/(2.0 + mach_t**2*(k_ratio - 1.0)))
            !pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
            pe_pstar = 1.0/mach_e*sqrt((k_ratio + 1.0)/(2.0 + mach_e**2*(k_ratio - 1.0)))
            pt_pe = pt_pstar/pe_pstar
            !if (pt_pe > press_ratio) then
            !    error_press = (pt_pe - press_ratio)/press_ratio
            !    mach_t = mach_t - abs(error_press)*0.00005
            !    m = m + 1
            !    if (mach_t >= mach_t_max .or. mach_t < 0.0) then
            !        !mach_t = 0.00000001
            !        !mach_t = 0.2999
            !        mach_t = mach_t_max - mach_t_factor
            !    endif
            !else
            error_press = (pt_pe - press_ratio)/press_ratio
            if (pt_pe > press_ratio .and. abs(error_press) > leak_conv_criterion) then
                !mach_t = mach_t + 0.0000001         
                !if (m < 5000) then
                !if (press_ratio < 2) then
                    !print'(1x,5F10.4)', mach_t, mach_e, error_press, press_ratio
                    mach_t = mach_t - abs(error_press)*0.001 !abs(error_press)*0.001
                    !mach_t = mach_t - 0.0001
                    !if (mach_t > (mach_t_max + 0.01) .or. mach_t < 0.0 .or. mach_t > 1.0) then
                    if (mach_t < 0.0 .or. mach_t > 1.0) then
                        !mach_t = 0.00000001
                        !mach_t = 0.2999
                        mach_t = mach_t_max+0.1 - 0.00001!abs(error_press)*0.001
                        !mach_t = mach_t_max - error_press*0.001
                        !mach_t = 0.0 + abs(error_press)*0.001
                    endif
                !endif
                m = m + 1
            elseif (pt_pe < press_ratio .and. abs(error_press) > leak_conv_criterion) then
                    !print'(1x,4F10.7)', mach_t, mach_e, error_press, press_ratio
                    mach_t = mach_t - abs(error_press)*0.00001
                    !if (mach_t > mach_t_max+0.01 .or. mach_t < 0.0 .or. mach_t > 1.0) then
                    if (mach_t < 0.0 .or. mach_t > 1.0) then
                        !mach_t = 0.00000001
                        !mach_t = 0.2999
                        mach_t = mach_t_max+0.1 - abs(error_press)*0.000001
                        !mach_t = 0.0 + abs(error_press)*0.000001
                    endif
                    m = m + 1
            endif
            !if (abs(error_press) > leak_conv_criterion) then
            !    if (m < 5000) then
            !        mach_t = mach_t - error_press*0.001!abs(error_press)*0.001 
            !        !mach_t = mach_t - 0.0001
            !        if (mach_t > mach_t_max+0.01 .or. mach_t < 0.0) then
            !            !mach_t = 0.00000001
            !            !mach_t = 0.2999
            !            mach_t = mach_t_max+0.01 - 0.00001
            !            !mach_t = mach_t_max
            !            !mach_t = 0.0 + abs(error_press)*0.001
            !        endif
            !    else
            !        print'(1x,4F10.7)', mach_t, mach_e, error_press, press_ratio
            !        mach_t = mach_t - error_press*0.0001 !(pt_pe - press_ratio)*0.0001!abs(error_press)*0.0001
            !        if (mach_t > mach_t_max+0.01 .or. mach_t < 0.0) then
            !            !mach_t = 0.00000001
            !            !mach_t = 0.2999
            !            mach_t = mach_t_max+0.01 - 0.0001!abs(error_press)*0.0001
            !            !mach_t = 0.0 + 
            !        endif
            !    endif
            !    m = m + 1
            !endif
            !!if (pt_pe <= press_ratio .and. mach_e > 0.97) then ! .and. abs(error_press) < 0.1) then
            !!    mach_t = mach_t_max
            !!    mach_e = mach_unity
            !!    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_e, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
            !!    goto 1056
            !!endif
            !if (m > 50000) then
            !    pause
            !    !print'(6F10.4)', mach_t, mach_e, pt_pe, press_ratio, error_press
            !    mach_t = mach_t - abs(pt_pe - press_ratio)*0.00000001!abs(error_press)*0.0001
            !        if (mach_t > mach_t_max+0.05 .or. mach_t < 0.0) then
            !            mach_t = mach_t_max - abs(pt_pe - press_ratio)*0.00000001!abs(error_press)*0.0001
            !        endif
            !endif
            ! ------------------------------------
            ! To account for long running
            ! ------------------------------------
            
            ! -----------------------
            ! If error happen, uncomment below
            ! -----------------------
            !if (m > 10000) then
            !    !print*, error_press, press_ratio, pt_pe
            !    !print*, mach_t, mach_e
            !    write(6,2982) press_ratio, pt_pe, mach_t, mach_e, error_press   ! for checking in-loop, on screen
            !endif
            
            if (m > 12000) then
                !print*, 'this is fanno width',m,leak_conv_criterion
                leak_conv_criterion = 2.*leak_conv_criterion
                m = 1
            endif
            ! ----- end here --------
            !write(6,2982) press_ratio, pt_pe, mach_t, mach_e, error_press   ! for checking in-loop, on screen
    enddo
    !write(99,2982) press_ratio, pt_pe, mach_t, mach_e, error_press  ! for checking
    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_e, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
1056 continue
return    
2982 format (7F8.4) 
    endsubroutine leakage_fanno_width

! -----------------------------------------------
! The bottom code is for small clearance (i.e. 20micron)
! updated on 19-02-2019
! 
! -----------------------------------------------
!subroutine leakage_fanno_width(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, length_flow, error_press, leak_conv_criterion, press_ratio, mach_t, clearance, l_fanno_eq, mach_e, dmdtheta_leak)
!    ! ----------------------------
!    ! Fanno flow without convergent duct
!    ! ----------------------------
!    implicit none
!    ! ----- input ------ !
!    double precision T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, length_flow
!    double precision error_press, leak_conv_criterion, press_ratio, mach_t, clearance, lambda_factor, l_f_assumed, l_fanno_eq
!    double precision dummy_A, dummy_B, dummy_C, dummy_D
!    double precision mach_e, dmdtheta_leak      ! --- output ---
!    double precision mach_e_a, mach_e_b, mach_e_c, f_mach_e_a, f_mach_e_b, f_mach_e_c, pt_pstar, pt_pe, pe_pstar
!    double precision mach_t_a, mach_t_b, mach_t_max, mach_t_factor
!    integer m, switch_case
!    m = 1
!    switch_case = 1
!    mach_t_factor = 0.00001
!    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
!    
!    ! ---- Equivalent friction channel length (l_f that makes exit flow is critical, which Mach number = 1)
!    dummy_D = lambda_factor*(l_fanno_eq)/(2.0*clearance)      ! dummy variables (equation left)
!    
!    mach_t_a = 0.00000001     
!    mach_t_b = 0.99999999
!    call bisection_method(switch_case, dummy_D, k_ratio, mach_t_a, mach_t_b, mach_t_max)
!    
!    pt_pstar = 1.0/mach_t_max*sqrt((k_ratio + 1.0)/(2.0 + mach_t_max**2*(k_ratio - 1.0)))
!    !pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
!    !pe_pstar = 1.0/mach_unity*sqrt((k_ratio + 1.0)/(2.0 + mach_unity**2*(k_ratio - 1.0)))
!    pt_pe = pt_pstar
!    
!    if (pt_pe < press_ratio) then
!        mach_e = 1.0
!        goto 1056
!    else
!        mach_t = mach_t_max+0.01
!        goto 1055   ! meaning not choked
!    endif
!1055 continue        
!    !call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_unity, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
!    do while (abs(error_press) > leak_conv_criterion)
!            ! --- Dummy variables for ease of calculation to solve for l_f_assumed
!            dummy_A = (1.0 - mach_t**2)/(k_ratio*mach_t**2)
!            dummy_B = (k_ratio + 1.0)/(2.*k_ratio)
!            dummy_C = (k_ratio + 1.0)*mach_t**2/(2.0 + (k_ratio - 1.0)*mach_t**2)
!            ! ---- Equivalent friction channel length (l_f that makes exit flow is critical, which Mach number = 1)
!            l_f_assumed = ((2.0*clearance)/lambda_factor)*(dummy_A + dummy_B*log(dummy_C))
!            dummy_D = lambda_factor*(l_f_assumed - l_fanno_eq)/(2.0*clearance)      ! dummy variables (equation left)
!
!            ! ----------------------------
!            ! Check Pressure Ratio
!            ! ----------------------------
!            !pt_pe = 1.0/mach_t*sqrt((k_ratio + 1.0)/(2.0 + mach_t**2*(k_ratio - 1.0)))
!            !pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
!            !pd_pe = pd_pt*pt_pe
!            !error_press = abs(pd_pe - press_ratio)/press_ratio
!            !if (error_press < leak_conv_criterion) then
!            !    goto 102
!            !endif
!            ! -------------------------------------------
!            ! Bisection method to find root of mach_e
!            ! equation is match when the error is between +- 1%
!            ! leakage_channel is the subroutine to calculate f_mach_e_a and f_mach_e_b (part of Bisection Method)
!            ! based on guessed value mach_e_a and mach_e_b.
!            ! by comparing result of a and b, the mid value is taken and calcualte again for compare
!            ! -------------------------------------------
!            mach_e_a = 1.0 
!            mach_e_b = mach_t
!            
!!            103         continue
!!            call leakage_channel(mach_e_a, k_ratio, dummy_D, f_mach_e_a)
!!            call leakage_channel(mach_e_b, k_ratio, dummy_D, f_mach_e_b)
!!            
!!            if (f_mach_e_a*f_mach_e_b .lt. 0.0) then
!!                mach_e_c = (mach_e_a + mach_e_b)/2.0
!!            else
!!                goto 102
!!            endif
!!            
!!            call leakage_channel(mach_e_c, k_ratio, dummy_D, f_mach_e_c)        ! to calculate the friction channel length
!!                
!!            if (f_mach_e_a*f_mach_e_c .lt. 0.0) then
!!                mach_e_b = mach_e_c
!!            else
!!                mach_e_a = mach_e_c
!!            endif
!!
!!            if (abs((mach_e_b - mach_e_a)) .ge. 0.000001) then !/mach_e_b
!!                goto 103
!!            else
!!                mach_e = mach_e_c       ! record down the final answer (which result in error +- 1%)
!!                goto 102
!!            endif
!!            
!!102         continue
!            call bisection_method(switch_case, dummy_D, k_ratio, mach_e_a, mach_e_b, mach_e)
!
!            pt_pstar = 1.0/mach_t*sqrt((k_ratio + 1.0)/(2.0 + mach_t**2*(k_ratio - 1.0)))
!            !pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
!            pe_pstar = 1.0/mach_e*sqrt((k_ratio + 1.0)/(2.0 + mach_e**2*(k_ratio - 1.0)))
!            pt_pe = pt_pstar/pe_pstar
!            !if (pt_pe > press_ratio) then
!            !    error_press = (pt_pe - press_ratio)/press_ratio
!            !    mach_t = mach_t - abs(error_press)*0.00005
!            !    m = m + 1
!            !    if (mach_t >= mach_t_max .or. mach_t < 0.0) then
!            !        !mach_t = 0.00000001
!            !        !mach_t = 0.2999
!            !        mach_t = mach_t_max - mach_t_factor
!            !    endif
!            !else
!            error_press = (pt_pe - press_ratio)/press_ratio
!            if (pt_pe > press_ratio) then
!                !mach_t = mach_t + 0.0000001         
!                !if (m < 5000) then
!                !if (press_ratio < 2) then
!                    !print'(1x,5F10.4)', mach_t, mach_e, error_press, press_ratio
!                    mach_t = mach_t - abs(error_press)*0.0001 !abs(error_press)*0.001
!                    !mach_t = mach_t - 0.0001
!                    if (mach_t > (mach_t_max + 0.01) .or. mach_t < 0.0) then
!                        !mach_t = 0.00000001
!                        !mach_t = 0.2999
!                        mach_t = mach_t_max+0.1 - 0.00001!abs(error_press)*0.001
!                        !mach_t = mach_t_max - error_press*0.001
!                        !mach_t = 0.0 + abs(error_press)*0.001
!                    endif
!                !endif
!                m = m + 1
!            elseif (pt_pe < press_ratio .and. abs(error_press) > leak_conv_criterion) then
!                    !print'(1x,4F10.7)', mach_t, mach_e, error_press, press_ratio
!                    mach_t = mach_t - abs(error_press)*0.00001
!                    if (mach_t > mach_t_max+0.01 .or. mach_t < 0.0) then
!                        !mach_t = 0.00000001
!                        !mach_t = 0.2999
!                        mach_t = mach_t_max+0.05 - abs(error_press)*0.000001
!                        !mach_t = 0.0 + abs(error_press)*0.000001
!                    endif
!                    m = m + 1
!            endif
!            !if (abs(error_press) > leak_conv_criterion) then
!            !    if (m < 5000) then
!            !        mach_t = mach_t - error_press*0.001!abs(error_press)*0.001 
!            !        !mach_t = mach_t - 0.0001
!            !        if (mach_t > mach_t_max+0.01 .or. mach_t < 0.0) then
!            !            !mach_t = 0.00000001
!            !            !mach_t = 0.2999
!            !            mach_t = mach_t_max+0.01 - 0.00001
!            !            !mach_t = mach_t_max
!            !            !mach_t = 0.0 + abs(error_press)*0.001
!            !        endif
!            !    else
!            !        print'(1x,4F10.7)', mach_t, mach_e, error_press, press_ratio
!            !        mach_t = mach_t - error_press*0.0001 !(pt_pe - press_ratio)*0.0001!abs(error_press)*0.0001
!            !        if (mach_t > mach_t_max+0.01 .or. mach_t < 0.0) then
!            !            !mach_t = 0.00000001
!            !            !mach_t = 0.2999
!            !            mach_t = mach_t_max+0.01 - 0.0001!abs(error_press)*0.0001
!            !            !mach_t = 0.0 + 
!            !        endif
!            !    endif
!            !    m = m + 1
!            !endif
!            !!if (pt_pe <= press_ratio .and. mach_e > 0.97) then ! .and. abs(error_press) < 0.1) then
!            !!    mach_t = mach_t_max
!            !!    mach_e = mach_unity
!            !!    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_e, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
!            !!    goto 1056
!            !!endif
!            !if (m > 50000) then
!            !    pause
!            !    !print'(6F10.4)', mach_t, mach_e, pt_pe, press_ratio, error_press
!            !    mach_t = mach_t - abs(pt_pe - press_ratio)*0.00000001!abs(error_press)*0.0001
!            !        if (mach_t > mach_t_max+0.05 .or. mach_t < 0.0) then
!            !            mach_t = mach_t_max - abs(pt_pe - press_ratio)*0.00000001!abs(error_press)*0.0001
!            !        endif
!            !endif
!            ! -----------------------
!            ! If error happen, uncomment below
!            ! -----------------------
!            !if (m > 50000) then
!            !    print*, error_press, press_ratio, pt_pe
!            !    print*, mach_t, mach_e
!            !endif
!            ! ----- end here --------
!    enddo
!    write(99,2982) press_ratio, pt_pe, mach_t, mach_e, error_press
!    call leakage_lambda_mflow(T_d, p_e, k_ratio, mach_e, R_specific, miu_refrigerant, clearance, length_flow, lambda_factor, dmdtheta_leak)
!1056 continue
!return    
!2982 format (5F8.4) 
!    endsubroutine leakage_fanno_width

! ---------------------------------
! end here
! ---------------------------------
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