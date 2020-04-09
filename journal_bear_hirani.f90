subroutine journal_bear_hirani(theta_1, F_cx, F_cy, F_resultant, eccr, att, h_min, p_max, Q_martin)
    implicit none
    include 'var_simulation_parameter.f90'
    include "var_operational_parameter.f90"
    include "var_operating_fluid_condition.f90"
    include 'var_physical_constants.f90'
    include 'var_main_dimensions.f90'
    include "var_geometrical.f90"
    include 'var_kinematics.f90'
    include "var_dynamic.f90"
    include "var_clearance.f90"
    ! ---- For simulation ----
    integer i,j,k,n, m
    integer, parameter :: N_DATA = 30, Max_ite = 100, p_data = 1000
    ! ------- Input variables -------
    double precision eccr_guess, att_guess, deccrdt_guess, dattdt_guess
    double precision, dimension (1:no_data+1) :: omega_av, theta_a1, theta_a2
    double precision conv_criterion, error_default
    common/conv_cri/conv_criterion, error_default
    ! ------- Dummy variables --------
    double precision, dimension (1:no_data+1) :: arg_F_result, omega_F_result
    double precision, dimension (1:no_data+1) :: dem_theta_a1
    double precision, dimension (1:N_DATA+1) :: eccr_ite, att_ite, deccrdt_ite, dattdt_ite
    double precision, dimension (1:p_data+1) :: p_max_dis
    double precision k_for_M, term_const_F, f1, f2, H, B1          ! f1, f2, H, B1 refers to Hirani paper
    !double precision LD_ratio, term_gs, term_go, term_O1, term_O2, term_O3, term_O, theta_O_max, theta_S_max, theta_P_max
    double precision theta_a12_dis
    double precision p_supply   
    ! ------- Output Variables --------
    double precision M_ecc, M_att
    double precision error_eccr, error_att
    double precision, dimension (1:no_data+1) :: eccr, att, deccrdt, dattdt
    double precision, dimension (1:no_data+1) :: Q_hydro, Q_press, Q_martin
    double precision, dimension (1:no_data+1) :: h_min, p_max       ! h_min = minimum oil film thickness, p_max = maximum pressure
    
    ! --------------------------------------------------------------------
    ! Part 1: calculate omega_F
    ! Step 
    !   1   . Find all the arguments of F_cx and F_cy (the angle of the resultant force) 
    !   2   . Find the change rate of arguments, i.e. dargdt = (arg (i+1) - arg(i))/(theta1(i+1) - theta1(i)) * omega_1
    !   3   . Store the dargdt
    ! atan(F_cy/F_cx), arg_F starts from the positive y-axis
    ! --------------------------------------------------------------------
    do i = 1,no_data+1
        if (F_cx(i) .eq. 0.0 .and. F_cy(i) .eq. 0.0) then
            arg_F_result(i) = 0.0       ! start from the 0 in UVC simulation (+ve x_axis)
        elseif (F_cx(i) .eq. 0.0 .and. F_cy(i) > 0.0) then
            arg_F_result(i) = 0.5*pi
        elseif (F_cx(i) .eq. 0.0 .and. F_cy(i) < 0.0) then
            arg_F_result(i) = 1.5*pi
        elseif (F_cx(i) > 0.0 .and. F_cy(i) > 0.0) then
            arg_F_result(i) = atan(F_cy(i) / F_cx(i))
        elseif (F_cx(i) > 0.0 .and. F_cy(i) < 0.0) then
            arg_F_result(i) = 2*pi - atan(abs(F_cy(i)) / F_cx(i))
        elseif (F_cx(i) < 0.0 .and. F_cy(i) > 0.0) then
            arg_F_result(i) = pi - atan(F_cy(i) / abs(F_cx(i)))
        elseif (F_cx(i) < 0.0 .and. F_cy(i) < 0.0) then
            arg_F_result(i) = pi + atan(abs(F_cy(i)) / abs(F_cx(i)))
        elseif (F_cx(i) > 0.0 .and. F_cy(i) .eq. 0.0) then
            arg_F_result(i) = 0.0
        elseif (F_cx(i) < 0.0 .and. F_cy(i) .eq. 0.0) then
            arg_F_result(i) = pi
        else
            arg_F_result(i) = atan(abs(F_cy(i)) / abs(F_cx(i)))
        endif
    enddo
    ! ------------ Correction for continuous arg_F_result -------------
    do i = 1, no_data+1
        arg_F_result(i) = arg_F_result(i) + 2.0*pi
        if (i >= 2 .and. i < no_data) then
            if (abs(arg_F_result(i) - arg_F_result(i-1)) > 1) then
                arg_F_result(i) = arg_F_result(i) - 2.0*pi
            endif
        endif
    enddo
    
    ! ----------- Omega of F resultant ---------------
    do i = 1, no_data+1
        if (i == no_data+1) then
            omega_F_result(i) = omega_F_result(i-1)
        else
            omega_F_result(i) = (arg_F_result(i+1) - arg_F_result(i))/(theta_1(i+1) - theta_1(i))*omega_1
        endif
    enddo       
    
    ! ===========================================================
    ! Part 2: Journal Bearing (Hirani)
    ! Hirani 1997-1999 paper
    ! Treat every forces as a single force
    ! -------------------------------------

    ! --------------------------------------------------------------------------------------------------
    ! Calculate average angular velocity of the journal and sleeve relative to the load vector (omega_av)
    ! omega_j is actually the shaft angular velocity, which is omega_1
    ! ---------------------------------------------------------------------------------------------------
    do i = 1, no_data+1
        omega_av(i) = 0.5*(omega_1 + omega_b) - omega_F_result(i)   ! [rad/s] Average angular velocity of the journal and sleeve relative to the load vector (omega_av)
    enddo
    
    ! ----- Calculation for constant dummy term --------------
    term_const_F = (cl_bear_s**2)/(6.*miu_oil*l_bearing*r_bearing**3)*1.d12
    
    !LD_ratio = l_bearing/(2.*r_bearing)
    do i = 1, no_data+1
        ! -------------------------------------
        ! Initialization: input guess data
        ! -------------------------------------
        eccr_guess      = 0.5		! eccr_guess	! initial input for Hirani journal bearing model
        att_guess       = 0.5		    ! att_guess,		! initial input for Hirani journal bearing model
        deccrdt_guess   = 0.5		! deccrdt_guess,	! initial input for Hirani journal bearing model
        dattdt_guess    = 0.5		! dattdt_guess	! initial input for Hirani journal bearing model
        error_eccr      = 1.0        ! error initilised to 1 (100%) 
        error_att       = 1.0         ! error initilised to 1 (100%)
        m = 1
    do while (error_eccr > conv_criterion .or. error_att > conv_criterion)
        ! ----------------------------------
        ! In this loop, the guessed eccentric ratio and attitude is assigned to the first point
        ! theta_a1 (to seperate it out with theta_1) is then evaluated, followed by theta_a2
        ! theta_a1 = atan(deccrdt/dem_theta_a1)
        ! ----------------------------------
        !print *, ' ---------------------------------------------------------------------------- '
        !print '(2x,A,I3)', ' Journal Bearing Iteration ', m
        do k = 1, N_DATA
            if (k == 1) then
                eccr_ite(k) = eccr_guess
                att_ite(k) = att_guess
                dem_theta_a1(i) = eccr_ite(k)*(omega_av(i) - dattdt_guess)  ! denominator of the theta 1 equation
                if (dem_theta_a1(i) == 0. .and. deccrdt_guess > 0.) then
                    theta_a1(i) = 0.5*pi
                elseif (dem_theta_a1(i) == 0. .and. deccrdt_guess < 0.) then
                    theta_a1(i) = - pi/2.
                elseif (dem_theta_a1(i) == 0. .and. deccrdt_guess == 0.) then
                    theta_a1(i) = 0.
                else
                    theta_a1(i) = atan( deccrdt_guess / dem_theta_a1(i) )
                endif
                if (dem_theta_a1(i) > 0.) then
                    theta_a2(i) = theta_a1(i) + pi
                    k_for_M = 1.0
                elseif (dem_theta_a1(i) < 0.) then
                    theta_a2(i) = theta_a1(i) - pi
                    k_for_M = - 1.0
                endif
            else
                dem_theta_a1(i) = eccr_ite(k)*(omega_av(i) - dattdt_ite(k-1))
                if (dem_theta_a1(i) == 0. .and. deccrdt_ite(k-1) > 0.) then
                    theta_a1(i) = pi/2.
                elseif (dem_theta_a1(i) == 0. .and. deccrdt_ite(k-1) < 0.) then
                    theta_a1(i) = -pi/2.
                elseif (dem_theta_a1(i) == 0. .and. deccrdt_ite(k-1) == 0.) then
                    theta_a1(i) = 0.0
                else 
                    theta_a1(i) = atan( deccrdt_ite(k-1) / dem_theta_a1(i) )
                endif

                if (dem_theta_a1(i) > 0.) then
                    theta_a2(i) = theta_a1(i) + pi
                    k_for_M = 1.0
                elseif (dem_theta_a1(i) < 0.) then
                    theta_a2(i) = theta_a1(i) - pi
                    k_for_M = - 1.0
                endif
            endif
            ! ------------------------------------
            ! This section calculate M_ecc and M_att 
            ! by using Weddle's formula
            ! ------------------------------------
            call journal_mecc_matt(eccr_ite(k), att_ite(k), theta_a1(i), k_for_M, l_bearing, r_bearing, M_ecc, M_att)
            
            deccrdt_ite(k) = 0.5*F_resultant(i)*term_const_F*M_ecc          ! calculation of deccrdt (gradient) based on current eccr and attitude ! 0.5 accounts for the resultant force (0.5 for upper bearing and 0.5 for lower bearing)
            dattdt_ite(k) = 0.5*F_resultant(i)*term_const_F*M_att/eccr_ite(k) + omega_av(i)
            
            eccr_ite(k+1) = eccr_ite(k) + deccrdt_ite(k)*(theta_1(i+1) - theta_1(i))/omega_1/N_DATA     ! Eccentric Ratio (calculate for next iteration)
            att_ite(k+1) = att_ite(k) + dattdt_ite(k)*(theta_1(i+1) - theta_1(i))/omega_1/N_DATA         ! Attitude angle    (next), time_step = time / N_DATA, time is /\theta_1//\omega_1
            
            if (i == no_data+1) then
                eccr_ite(k+1) = eccr_ite(k) + deccrdt_ite(k) * ( theta_1(i) - theta_1(i-1) ) / omega_1/N_DATA      ! Eccentric Ratio (next)
                att_ite(k+1) = att_ite(k) + dattdt_ite(k) * ( theta_1(i) - theta_1(i-1) ) / omega_1/N_DATA         ! Attitude angle    (next)       
            endif
            
            ! Make the attitude angle below 360degree
            if (att_ite(k+1) > 2.0*pi) then
                n = floor(att_ite(k+1)/(2.0*pi))
                att_ite(k+1) = att_ite(k+1) - n*2.0*pi
            elseif (att_ite(k+1) < - 2.0*pi) then
                n = floor(abs(att_ite(k+1))/(2.0*pi))
                att_ite(k+1) = - (abs(att_ite(k+1)) - n*2.0*pi)
                !att_ite(k+1) = att_ite(k+1) + n*2.*pi
            endif
        enddo

        error_eccr = abs(eccr_ite(N_DATA) - eccr_ite(1))/eccr_ite(1)
        error_att = abs(att_ite(N_DATA) - att_ite(1))/att_ite(1)
        if (i == no_data/2) then
            print *, ' ---------------------------------------------------------------------------- '
            print '(2x,A,F10.2,A)', 'Error in Eccentricity Ratio = ', error_eccr*100.0, ' % '
            print '(2x,A,F10.2,A)', 'Error in Attitude Angle     = ', error_att*100.0, ' % '
            print *, ' ---------------------------------------------------------------------------- '
        endif
        m = m + 1
        if ( m == Max_ite ) then
            !print *, ' '
            !print *, ' Maximum iteration reached, end iteration now '
            !print *, ' ---------------------------------------------------------------------------- '
            !m = 0
            goto 168
        endif
        ! ------ assign last point to the first point (guess point) of next iteration if not yet converged ------
        eccr_guess = eccr_ite(N_DATA)
        deccrdt_guess = deccrdt_ite(N_DATA)
        att_guess = att_ite(N_DATA)
        dattdt_guess = dattdt_ite(N_DATA)
        
    enddo
168         continue 
        ! ------ store the converged point ------- 
        eccr(i) = eccr_ite(N_DATA)
        deccrdt(i) = deccrdt_ite(N_DATA)
        att(i) = att_ite(N_DATA)
        dattdt(i) = dattdt_ite(N_DATA)
    enddo
    ! ------------------------------------------
    ! Part 3: Calcualte P_max and h_min
    ! 1     . Determine necessary dummy variables to find theta_P_max       
    ! 2     . determine theta_P_max and sub to find P_max
    ! 3     . Start from finding pressure at every point between theta_a1 and theta_a2 under every step i
    ! 4     . Assign maximum Pressure between theta_a1 and theta_a2 to p_max(i)
    ! ------------------------------------------
    do i = 1, no_data + 1
    theta_a12_dis = theta_a1(i)     
    do j = 1, p_data     
        f1 = eccr(i)*(omega_av(i) - dattdt(i))*sin(theta_a12_dis) - deccrdt(i)*cos(theta_a12_dis)
        f2 = (1.0 + eccr(i)*cos(theta_a12_dis))**3
        H = 1.0 + eccr(i)*cos(theta_a12_dis)
        B1 = H*(1.0 + H)/(l_bearing*1.d-3/(r_bearing*1.d-3))**2
        p_max_dis(j) = 1.5*miu_oil*(l_bearing*1.d-3/cl_bear_s)**2*f1/((1.0 + 1.0/(4.*B1))*f2)
        theta_a12_dis = theta_a12_dis + (theta_a2(i) - theta_a1(i))/p_data
    enddo
    h_min(i) = cl_bear_s*(1.0-eccr(i))  ! radial, not diametral
    p_max(i) = maxval(p_max_dis)
    enddo
    
    ! ------------------------------------------
    ! Part 4: Calcualte Minimum Required Oil Flow 
    ! 1     . Q_hydro and Q_press -- Hydrodynamic flow and feed pressure flow
    ! 2     . Q_hydro = 2*radial_clearance*l_bearing*bear_radius*(eccr*(dattdt - omega_av)*cos(theta_a1) - deccrdt*(sin(theta_a1)))    Equation from Hirani (2000)
    ! 3     . Q_press is calculated based on H.J. Kim (2003) analysis       (since not a circumference groove, it is just a normal clearance)
    ! 4     . Q_martin - empirical solution from Martin (1993)
    ! ------------------------------------------
    p_supply = p_disc - p_suc       ! [kPa] supply pressure -- assume to be pressure difference between discharge and suction chamber
    do i = 1, no_data + 1
        Q_hydro(i) = 2.*cl_bear_s*l_bearing*r_bearing*1.d-6*(eccr(i)*(dattdt(i) - omega_av(i))*cos(theta_a1(i)) - deccrdt(i)*sin(theta_a1(i)))      ! hydrodynamic flow
        Q_hydro(i) = abs(Q_hydro(i))    ! [m3/s]
        Q_press(i) = pi*p_supply*1000.0*r_shaft*1.d-3*cl_bear_s**3/(6.0*miu_oil*l_bearing*1.d-3)*(1.0 + 1.5*eccr(i)**2)     ! [m3/s] pressure driven feed flow
        Q_martin(i) = Q_hydro(i) + Q_press(i) - 0.3*(Q_hydro(i)*Q_press(i))**0.5       ! [m3/s] Martin Flow (empirical equation), Martin (1993)
    enddo
    
    
    
    
    print *, ' ---------------------------------------------------------------------------- '  
    write(6,2989) " Maximum Pressure           = ", maxval(p_max)/1.d6, " MPa"
    write(6,2989) " Minimum Oil Film Thickness = ", minval(h_min)*1.d6, " micron-m"
    write(6,2989) " Minimum Inst Required Oil Flow  = ", maxval(Q_martin)*1.d6, " cc/s"
    print *, ' ---------------------------------------------------------------------------- '
    
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Journal bearing (Plain bearing) Analysis "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) " Maximum Pressure             = ", maxval(p_max)/1.d6, " MPa"
    write (113,2989) " Minimum Oil Film Thickness   = ", minval(h_min)*1.d6, " micron-m"
    write (113,2989) " Minimum Inst Required Oil Flow    = ", maxval(Q_martin)*1.d6, " cc/s"
    write (113,*) " "
    
    
2981 format (15A25)
2982 format (F25.4, 14ES25.6)    
2983 format (14ES25.6) 
2989 format (2x,A,F12.4,A)  
    endsubroutine journal_bear_hirani
    
    
    !term_gs = exp((1.0 - eccr(i))**3)
            !term_go = 1. + eccr(i)*LD_ratio**1.2*(exp(eccr(i)**5) - 1.0)
            !term_O1 = LD_ratio**2*term_gs/(3.*term_go)
            !term_O2 = (2. + eccr(i)**2)**4*(12*eccr(i)**2 + (2. + eccr(i)**2)*(1. - sqrt(1. + 24.*eccr(i)**2)))
            !term_O3 = eccr(i)**2*(1. - eccr(i)**2)**2*(2. - eccr(i)**2)*(9. - sqrt(1. + 24.*eccr(i)**2))**2
            !term_O = term_O1*term_O2/term_O3
            !theta_O_max = acos((1. - sqrt(1. + 24.*eccr(i)**2))/(4. * eccr(i)))
            !theta_S_max = acos(-3*eccr(i) / (2. + eccr(i)**2))
            !theta_P_max = theta_S_max + (theta_O_max - theta_S_max)*(1./(1.-term_O))
    
            !! ------ store the converged points ------- 
        !eccr_last(k) = eccr(no_data+1)
        !deccrdt_last(k) = deccrdt(no_data)
        !att_last(k) = att(no_data+1)
        !dattdt_last(k) = dattdt(no_data)
        !
        !! ------ assign last point to the first point (guess point) ------
        !eccr_guess = eccr(no_data+1)
        !deccrdt_guess = deccrdt(no_data)
        !att_guess = att(no_data+1)
        !dattdt_guess = dattdt(no_data)
        !k = k + 1
        !error_eccr = abs(eccr(no_data+1) - eccr(1))
        !error_att = abs(att(no_data+1) - att(1))
        !print *, ' ---------------------------------------------------------------------------- '
        !print '(2x,A,F10.2,A)', 'Error in Eccentricity Ratio = ', error_eccr*100, ' % '
        !print '(2x,A,F10.2,A)', 'Error in Attitude Angle     = ', error_att*100, ' % '
        !!print *, ' ---------------------------------------------------------------------------- '
        !if ( k == 50 ) then
        !    print *, ' '
        !    print *, ' Maximum iteration achieved, end iteration now '
        !    print *, ' ---------------------------------------------------------------------------- '
        !    goto 169
        !endif
    
                    
                !if ((theta_a1(i) < 0 .and. theta_a1(i-1) > 0) .or. (theta_a1(i) > 0. .and. theta_a1(i-1)< 0.)) then
                !    if (theta_a1(i) < 0.0) then
                !        theta_a1(i) = pi + theta_a1(i)
                !        k_for_M = +1
                !    else
                !        theta_a1(i) = - pi + theta_a1(i)
                !        k_for_M = -1
                !    endif
                !endif
                