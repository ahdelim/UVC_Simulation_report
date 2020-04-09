subroutine leakage_m(theta_1, theta_2, p_scv, t_scv, rho_scv, p_dcv, t_dcv, cv_dcv, cp_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, l_v, cl_rotor_rad_dy, dmdtheta1_leak_rad_ro, dmdtheta1_leak_vef, dmdtheta1_leak_vs, dmdtheta1_leak_ref, dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d)
                                                                                                                
    implicit none
    ! ----------- Simulation parameters and constants --------
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"
    include "var_clearance.f90"
    include "var_main_dimensions.f90"
    include "var_operating_fluid_condition.f90"
    ! ------------ input variables ------------ !
    !double precision, parameter :: leak_conv_criterion = 0.05
    double precision leak_conv_criterion, lock_conv_crit
    double precision, dimension (1:no_data+1) :: theta_1, theta_2, p_scv, t_scv, p_dcv, t_dcv, cv_dcv, cp_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, rho_scv
    double precision, dimension (1:no_data+1) :: l_v, cl_rotor_rad_dy
    ! ------------ Output variables ----------- !
    include "var_leakage.f90"
    ! ------------ Dummy variables ------------ !
    integer i,m
    !integer, parameter :: max_ite = 100
    double precision, parameter ::  mach_unity = 1.0000
    double precision press_ratio, pt_pe, pd_pe, pd_pt, pt_pstar, pe_pstar, Re_number, lambda_factor, k_ratio, miu_refrigerant
    double precision error_press_ro, error_press_vef, error_press_ref
    double precision l_f_rad_ro, l_v_ratio, l_v_leakage, l_f_vef, l_eq_ref, vel_ref, w_eq_vane
    double precision cl_up_low_sum
    double precision mach_t_ro, mach_t_vef, mach_t_ref, mach_e_ro, mach_e_vef, mach_e_ref      ! for Fanno flow
    double precision p_avg, area_orf    ! for orifice
    double precision sum_a, sum_b, sum_c, sum_d
    sum_a = 0.d0
    sum_b = 0.d0
    sum_c = 0.d0
    sum_d = 0.d0

    ! ---------------------------------
    ! Leakage model based on Kuan Tai's thesis & Yanagisawa et al. paper
    ! Total 5 leakage paths in NVC
    ! 1 -   Vane side 
    ! 2 -   Vane Endface
    ! 3 -   Rotor Endface (exit to vane chamber with assumed half of compression chamber pressure)
    ! 4 -   Radial (rotor and cylinder) clearance 
    ! exit = suction chamber
    ! ---------------------------------
    ! **********************************************************
    ! Constant term definition 
    ! ------------------------
    ! Fanno Flow case,
    ! clearance of upper endface and lower endface is sum together
    ! Viscosity is treated as constant thoughout the operation, take average
    ! Real equivalent channel length calculation
    ! ------------------------
    cl_up_low_sum = cl_upend + cl_lowend
    miu_refrigerant = miu_disc
    !l_f_rad_ro = 2.0*pi*cl_rad_ro*r_oc*1.d-3/((e*1.d-3)*sqrt(1.0 - (1.0 - cl_rad_ro/(e*1.d-3))**2))     ! [m] equivalent channel length of the radial clearance between rotor and outer cylinder
    w_eq_vane = w_v_ro*1.d-3
    ! -----------------------------------------------------------
    ! 2 -   Vane Endface
    ! 4 -   Radial (rotor and cylinder) clearance 
    ! -----------------------------------------------------------
    mach_t_ro = 0.000001   ! initialise (Guess) mach number at throat
    mach_t_vef = 0.000001   ! initialise (Guess) mach number at throat
    !mach_t_ref = 0.00010000
    !mach_t_ro = 0.9999  ! initialise (Guess) mach number at throat
    !mach_t_vef = 0.9999   ! initialise (Guess) mach number at throat
    !mach_t_ref = 0.9999
    ! -----------------------------------
    ! Orifice model 
    ! 1.    Vane Side
    ! -----------------------------------
    area_orf = cl_vs*l_com*1.d-3      ! [m2]
    
    ! **********************************************************
    ! Loop starts..
    ! -----------------------------------
    do i = 1, no_data! + 1
        ! -----------------------------------
        ! Initialization
        ! -----------------------------------
        l_f_rad_ro = 2.0*pi*cl_rotor_rad_dy(i)*r_oc*1.d-3/((e*1.d-3)*sqrt(1.0 - (1.0 - cl_rotor_rad_dy(i)/(e*1.d-3))**2))   ! Dynamic clearance (Yanagisawa)
        leak_conv_criterion = 0.1
        lock_conv_crit = leak_conv_criterion
        press_ratio = p_dcv(i)/p_scv(i)
        k_ratio = cp_dcv(i)/cv_dcv(i)
        p_avg = (p_scv(i) + p_dcv(i))/2.0   ! [kPa] Vane housing chamber pressure is treated as average pressure between suction and discharge chamber
        !if (i >= 3) then
        !    mach_t_vef = mach_t_vef + mach_t_array(i-1) - mach_t_array(i-2)
        !endif
        
        mach_e_ro = 0.00000001      ! initialise mach number at exit
        mach_e_vef = 0.00000001     ! initialise mach number at exit of vane endface
        mach_t_ro = mach_t_ro + 0.0000001 ! initialise (Guess) mach number at throat
        mach_t_vef = mach_t_vef + 0.0000001   ! initialise (Guess) mach number at throat
        !mach_e_ref = 0.00000001     ! initialise mach number at exit of rotor endface
        !m = 1           ! record number of iteration for each loop
        
        ! ----------------------------------------------------------
        ! equivalent channel length of vane endface is different from radial clearance
        ! when l_v > l_vh, l_v_ratio is used to calculate the eq channel length
        ! else, eq channel length will be w_v_max
        ! ----------------------------------------------------------
        l_v_leakage = l_v(i) + l_vs_ro
        if (l_v(i) > l_vh) then
            l_v_ratio = (l_v_leakage - l_vh)/l_v_leakage
            l_f_vef = (l_v_ratio*w_v_ro + (1.0 - l_v_ratio)*w_v_max)*1.d-3      ! Equivalent channel length
        else
            l_f_vef = w_v_max*1.d-3
        endif

        ! --------- Initialise Error -----------------        
        error_press_ro = 1.0000   ! initialise Error for rotor radial
        error_press_vef = 1.0000    ! initialise error for vane endface        
        !error_press_ref = 1.0000     ! Initialise error for vane side
        ! ------------------------------------
        ! Leakage calculation of vane side based on Orifice Model
        ! ------------------------------------
     !   if (p_avg < p_dcv(i)) then
     !       call leakage_orifice(omega_1, area_orf, coef_discport, p_avg, rho_dcv(i), h_dcv(i), s_dcv(i), dmdtheta1_leak_vs(i))     ! orifice subroutine for leakage only
	    !else
            dmdtheta1_leak_vs(i) = 0.0          ! Vane side is pushed toward vane housing vane, which closed the vane side clearance
        !endif
    
        if (press_ratio < 1.0) then
            dmdtheta1_leak_rad_ro(i) = 0.0
            dmdtheta1_leak_vef(i) = 0.0
            dmdtheta1_leak_ref(i) = 0.0
        else
            ! -------------------------------------
            ! leakage_fanno -- is used to calculate fanno flow and mass flow rate under this condition
            ! leakage_lambda_mflow -- is a subroutine to find lambda_factor, and leakage mass flow rate of the specific path
            ! lambda_factor -- is used later for calculation of dummy variables in subroutine leakage_fanno
            ! leakage_fanno -- Fanno flow through convergent duct
            ! leakage_fanno_width -- Fanno flow without convergent duct
            ! --------------------------------------------
            ! 3 -   Rotor Endface (exit to vane chamber with assumed half of compression chamber pressure)
            !       Analysed by CFD approach (direct use the result for the lumped numerical simulation)
            ! CFD analysis of Rotor Endface clearance leakage done by W.C. Poh  (updated 23-03-2018)
            ! dmdt = density*length_equivalent*clearance*velocity
            ! Velocity and Equivalent Length of rotor endface are found using CFD Ansys Fluent, by Ideal Gas Model
            ! Velocity : for whole cycle y = 0.0119*theta_1(i)**6 - 0.1004*theta_1(i)**5 - 0.3521*theta_1(i)**4 + 4.3684*theta_1(i)**3 - 8.3548*theta_1(i)**2 + 7.8128*theta_1(i) - 0.1256
            ! otherwise, in piecewise function
            ! Compression process : y = - 0.1899*theta_1(i)**6 + 2.4622*theta_1(i)**5 - 12.081theta_1(i)**4 + 27.718*theta_1(i)**3 - 27.378*theta_1(i)**2 + 12.206*theta_1(i) + 0.0046
            ! Discharge process : y = -7.4523*theta_1(i)**3 + 117.21*theta_1(i)**2 - 626.19*theta_1(i) + 1156.7
            ! L_eq : y = 0.074585x^0.253195, ! or 0.08074968*(press_ratio - 1.0)**0.346300667
            ! --------------------------------------------
            
            
            !vel_ref = 
            !if (theta_1(i)*180.0/pi <= 245.0) then
            !    vel_ref = - 0.1899*theta_1(i)**6 + 2.4622*theta_1(i)**5 - 12.081*theta_1(i)**4 + 27.718*theta_1(i)**3 - 27.378*theta_1(i)**2 + 12.206*theta_1(i) + 0.0046      ! piecewise velocity equation (NVC)
            !else
            !    vel_ref = -7.4523*theta_1(i)**3 + 117.21*theta_1(i)**2 - 626.19*theta_1(i) + 1156.7
            !endif
            !vel_ref = 0.0115*theta_1(i)**6 - 0.0825*theta_1(i)**5 - 0.5668*theta_1(i)**4 + 5.3483*theta_1(i)**3 - 10.158*theta_1(i)**2 + 9.5356*theta_1(i) - 0.1046     ! for Swing Compressor
            !l_eq_ref = l_eq_ref*1000.      ! convert [m] to [mm]
            !l_eq_ref = 0.077232162*(press_ratio - 1.0)**0.371304931     ! Equation from CFD for Swing Compressor
            ! -------------------------------------------------
            ! Wai Chang first model (2017)
            ! -------------------------------------------------
            !vel_ref = 0.0119*theta_1(i)**6 - 0.1004*theta_1(i)**5 - 0.3521*theta_1(i)**4 + 4.3684*theta_1(i)**3 - 8.3548*theta_1(i)**2 + 7.8128*theta_1(i) - 0.1256        ! full velocity equation (NVC)
            !l_eq_ref = 0.08074968*(press_ratio - 1.0)**0.346300667      ! Equation from CFD simulation (NVC) (suction chamber length)
            !dmdtheta1_leak_ref(i) = rho_suc*vel_ref*l_eq_ref*cl_up_low_sum/omega_1      ! if using CFD result completely
            ! =================================================
            ! -------------------------------------------------
            ! Wai Chang SECOND model (2019)
            ! -------------------------------------------------
            vel_ref = 71.921*log(press_ratio) - 2.9277      ! Velocity Equation vs press_ratio from Wai Chang model
            l_eq_ref = abs(2*r_ro*sin(0.5*theta_2(i))*1.d-3)     ! Chord length
            !write(99,*), theta_1(i)*180.0/pi, l_eq_ref
            dmdtheta1_leak_ref(i) = rho_scv(i)*vel_ref*l_eq_ref*cl_up_low_sum/omega_1      ! if using CFD result completely
            ! -------------------------------------------------
            !dmdtheta1_leak_ref(i) = (1./12.)*rho_scv(i)*pi*(cl_upend**3 + cl_lowend**3)*(p_dcv(i) - p_scv(i))*1000.0/(miu_oil*log(r_ro/r_roi))/omega_1      ! Yanagisawa model
            call leakage_fanno(t_dcv(i), p_scv(i), k_ratio, mach_unity, R_specific, miu_refrigerant, l_com, error_press_ro, leak_conv_criterion, press_ratio, mach_t_ro, cl_rotor_rad_dy(i), l_f_rad_ro, mach_e_ro, dmdtheta1_leak_rad_ro(i))        ! Rotor Radial clearance leakage
            leak_conv_criterion = lock_conv_crit
           ! if (cl_up_low_sum >= 25.0) then
            call leakage_fanno_width(t_dcv(i), p_scv(i), k_ratio, mach_unity, R_specific, miu_refrigerant, l_v_leakage, error_press_vef, leak_conv_criterion, press_ratio, mach_t_vef, cl_up_low_sum, l_f_vef, mach_e_vef, dmdtheta1_leak_vef(i))       ! Vane endface clearance leakage
           ! else 
              ! !!! call leakage_fanno_width_s_clr(t_dcv(i), p_scv(i), k_ratio, mach_unity, R_specific, miu_refrigerant, l_v_leakage, error_press_vef, leak_conv_criterion, press_ratio, mach_t_vef, cl_up_low_sum, l_f_vef, mach_e_vef, dmdtheta1_leak_vef(i))       ! Vane endface clearance leakage
            !endif
            leak_conv_criterion = lock_conv_crit    
            !call leakage_fanno_width(t_dcv(i), p_scv(i), k_ratio, mach_unity, R_specific, miu_refrigerant, l_eq_ref, error_press_ref, leak_conv_criterion, press_ratio, mach_t_ref, cl_up_low_sum, w_eq_vane, mach_e_ref, dmdtheta1_leak_ref(i))   ! Rotor endface clearance, calculate with ansys simulation
            !write(99,*) theta_1(i), mach_t_vef, mach_e_vef
        endif
        
        
        !if (press_ratio < 1.0) then
        !    dmdtheta1_leak_ref(i) = 0.0
        !else
        !    !vel_ref = 0.0119*theta_1(i)**6 - 0.1004*theta_1(i)**5 - 0.3521*theta_1(i)**4 + 4.3684*theta_1(i)**3 - 8.3548*theta_1(i)**2 + 7.8128*theta_1(i) - 0.1256
        !    if (theta_1(i)*180.0/pi <= 240.0) then
        !        vel_ref = - 0.1899*theta_1(i)**6 + 2.4622*theta_1(i)**5 - 12.081*theta_1(i)**4 + 27.718*theta_1(i)**3 - 27.378*theta_1(i)**2 + 12.206*theta_1(i) + 0.0046
        !    else
        !        vel_ref = -7.4523*theta_1(i)**3 + 117.21*theta_1(i)**2 - 626.19*theta_1(i) + 1156.7
        !    endif
        !    !vel_ref = 0.0115*theta_1(i)**6 - 0.0825*theta_1(i)**5 - 0.5668*theta_1(i)**4 + 5.3483*theta_1(i)**3 - 10.158*theta_1(i)**2 + 9.5356*theta_1(i) - 0.1046     ! for Swing Compressor
        !    l_eq_ref = 0.08074968*(press_ratio - 1.0)**0.346300667      ! Equation from CFD simulation (NVC)
        !    !l_eq_ref = 0.077232162*(press_ratio - 1.0)**0.371304931     ! Equation from CFD for Swing Compressor
        !    dmdtheta1_leak_ref(i) = rho_suc*vel_ref*l_eq_ref*cl_up_low_sum/omega_1
        !endif
        ! -------- for checking purpose -------------
        !write(99,'(6F15.8)'), theta_1(i)*180/pi, mach_t_ro, mach_e_ro, mach_t_vef, mach_e_vef
    enddo
    
    ! ---------------------------------------
    ! Data correction for the last point..
    ! ---------------------------------------
    dmdtheta1_leak_rad_ro(no_data+1) = dmdtheta1_leak_rad_ro(no_data)
    dmdtheta1_leak_vef(no_data+1) = dmdtheta1_leak_vef(no_data)
    dmdtheta1_leak_vs(no_data+1) = dmdtheta1_leak_vs(no_data)
    dmdtheta1_leak_ref(no_data+1) = dmdtheta1_leak_ref(no_data)
    
    ! ---------------------------------------
    ! Assign leakage mass into leakage into chamber
    ! or out of chamber, unit in (kg/rad), because the simulation is run based on 1 cycle only
    ! leakage flow is multiplied with a coefficient to account for oil blockage
    ! ---------------------------------------
    do i = 1, no_data + 1
        dmdtheta1_leak_rad_ro(i)  = coef_rad*dmdtheta1_leak_rad_ro(i) 
        dmdtheta1_leak_vef(i) = coef_vef*dmdtheta1_leak_vef(i)
        dmdtheta1_leak_ref(i) = coef_ref*dmdtheta1_leak_ref(i)
        !if (theta_1(i)*180.0/pi >= theta_disc_end*180.0/pi + 5) then
        !    dmdtheta1_leak_rad_ro(i)  = 0.3*dmdtheta1_leak_rad_ro(i) 
        !    dmdtheta1_leak_vef(i) = 0.3*dmdtheta1_leak_vef(i)
        !    dmdtheta1_leak_ref(i) = 0.3*dmdtheta1_leak_ref(i)
        !endif
        dmdtheta1_leak_s(i) = dmdtheta1_leak_rad_ro(i) + dmdtheta1_leak_vef(i) + dmdtheta1_leak_vs(i) + dmdtheta1_leak_ref(i)      ! [kg/rad]
        dedtheta1_leak_s(i) = dmdtheta1_leak_s(i)*h_dcv(i)                          ! [kg/rad]
        dmdtheta1_leak_d(i) = - (dmdtheta1_leak_rad_ro(i) + dmdtheta1_leak_vef(i) + dmdtheta1_leak_vs(i) + dmdtheta1_leak_ref(i))
        dedtheta1_leak_d(i) = dmdtheta1_leak_d(i)*h_dcv(i)
    enddo

    do i = 1, no_data
        sum_a = sum_a + 0.5*(dmdtheta1_leak_rad_ro(i+1) + dmdtheta1_leak_rad_ro(i))*(theta_1(i+1) - theta_1(i))
        sum_b = sum_b + 0.5*(dmdtheta1_leak_vef(i+1) + dmdtheta1_leak_vef(i))*(theta_1(i+1) - theta_1(i))
        sum_c = sum_c + 0.5*(dmdtheta1_leak_ref(i+1) + dmdtheta1_leak_ref(i))*(theta_1(i+1) - theta_1(i))
        sum_d = sum_d + 0.5*(dmdtheta1_leak_s(i+1) + dmdtheta1_leak_s(i))*(theta_1(i+1) - theta_1(i))
    enddo
    ! ---------------------------------------
    ! Average internal leakage per cycle
    ! ---------------------------------------
    avrg_leak_rad_ro = sum_a*freq!sum(dmdtheta1_leak_rad_ro)/(no_data+1)*omega_1   ! kg/s
    avrg_leak_vef = sum_b*freq !sum(dmdtheta1_leak_vef)/(no_data+1)*omega_1     ! kg/s
    avrg_leak_vs = sum(dmdtheta1_leak_vs)/(no_data+1)*omega_1       ! kg/s
    avrg_leak_ref = sum_c*freq !sum(dmdtheta1_leak_ref)/(no_data+1)*omega_1     ! kg/s
    avrg_leakage_mass = sum_d*freq !sum(dmdtheta1_leak_s)/(no_data+1)*omega_1   ! kg/s
    
    print *, ' ---------------------------------------------------------------------------- '
    !print '(2x,A,F10.4,A)', 'Avrg. Radial Clearance Leakage   = ', sum(dmdtheta1_leak_rad_ro)/(no_data+1)*omega_1*1000, ' g/s '
    !print '(2x,A,F10.4,A)', 'Avrg. Vane Endface Leakage       = ', sum(dmdtheta1_leak_vef)/(no_data+1)*omega_1*1000, ' g/s '
    !!print '(2x,A,F10.4,A)', 'Avrg. Vane Side Leakage          = ', sum(dmdtheta1_leak_vs)/(no_data+1)*omega_1*1000, ' g/s'
    !print '(2x,A,F10.4,A)', 'Avrg. Rotor Endface Leakage      = ', sum(dmdtheta1_leak_ref)/(no_data+1)*omega_1*1000, ' g/s '
    !print '(2x,A,F10.4,A)', 'Avrg. Total Leakage              = ', sum(dmdtheta1_leak_s)/(no_data+1)*omega_1*1000, ' g/s '
    print '(2x,A,F10.4,A)', 'Avrg. Radial Clearance Leakage   = ', avrg_leak_rad_ro*1000, ' g/s '
    print '(2x,A,F10.4,A)', 'Avrg. Vane Endface Leakage       = ', avrg_leak_vef*1000, ' g/s '
    !print '(2x,A,F10.4,A)', 'Avrg. Vane Side Leakage          = ', avrg_leak_vs*1000, ' g/s'
    print '(2x,A,F10.4,A)', 'Avrg. Rotor Endface Leakage      = ', avrg_leak_ref*1000, ' g/s '
    print '(2x,A,F10.4,A)', 'Avrg. Total Leakage              = ', avrg_leakage_mass*1000, ' g/s '
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' '

    
2990 format (2x,A,f12.4,A)
    endsubroutine leakage_m

! -------------------------
! iterates mach_e to match the equation
! -------------------------
            
!            do mach_e = 0.0,1.0,0.0001      
!                dummy_A_e = (1.0 - mach_e**2)/(k_ratio*mach_e**2)
!                dummy_B_e = (k_ratio + 1.0)/(2.*k_ratio)
!                dummy_C_e = (k_ratio + 1.0)*mach_e**2/(2.0 + (k_ratio - 1.0)*mach_e**2)
!                dummy_D_e = (dummy_A_e + dummy_B_e*log(dummy_C_e))
!        
!                if ((dummy_D_e - dummy_D)/dummy_D <= 0.01 .and. (dummy_D_e - dummy_D)/dummy_D >= - 0.01) then
!                    pt_pstar = 1.0/mach_t*sqrt((k_ratio + 1.0)/(2.0 + mach_t**2*(k_ratio - 1.0)))
!                    pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
!                    pe_pstar = 1.0/mach_e*sqrt((k_ratio + 1.0)/(2.0 + mach_e**2*(k_ratio - 1.0)))
!                    pd_pe = pd_pt*pt_pstar/pe_pstar
!                    go to 101   
!                endif
!101         continue

! ---------------------------------------
! Past code
! ---------------------------------------
    !temp_e = t_dcv(i)/(1.0 + mach_unity**2*(k_ratio - 1.0)/2.0)
    !velo_e = mach_unity*sqrt(k_ratio*R_specific*temp_e)      ! velocity in [m/s]
    !dmdtheta1_leak_rad_ro(i) = cl_rad_ro*l_com*1.d-3*velo_e*p_scv(i)*1000.0/(temp_e*R_specific)        ! leakage mass flow through the radial clearance
    !Re_number = 2.0/(miu_oil*l_com*1.d-3)*dmdtheta1_leak_rad_ro(i)      ! Reynold number
    !if (Re_number > 3560.0) then
    !    lambda_factor = 0.3164/Re_number**0.25
    !else
    !    lambda_factor = 96.0/Re_number
    !endif
    
    !call leakage_lambda_mflow(t_dcv(i), p_scv(i), k_ratio, mach_unity, R_specific, miu_oil, cl_rad_ro, l_com, lambda_factor, dmdtheta1_leak_rad_ro(i))
!    do while (error_press > leak_conv_criterion)
!            ! --- Dummy variables for ease of calculation to solve for l_f_assumed
!            dummy_A = (1.0 - mach_t**2)/(k_ratio*mach_t**2)
!            dummy_B = (k_ratio + 1.0)/(2.*k_ratio)
!            dummy_C = (k_ratio + 1.0)*mach_t**2/(2.0 + (k_ratio - 1.0)*mach_t**2)
!            ! ---- Equivalent friction channel length (l_f that makes exit flow is critical, which Mach number = 1)
!            l_f_assumed = ((2.0*cl_rad_ro)/lambda_factor)*(dummy_A + dummy_B*log(dummy_C))
!            dummy_D = lambda_factor*(l_f_assumed - l_f_rad_ro)/(2.0*cl_rad_ro)      ! dummy variables (equation left)
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
!            ! based on guessed value mach_e_a and mach_e_b
!            ! by comparing result of a and b, the mid value is taken and calcualte again for compare
!            ! -------------------------------------------
!            mach_e_a = 0.0000001
!            mach_e_b = 0.9999999
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
!            call leakage_channel(mach_e_c, k_ratio, dummy_D, f_mach_e_c)
!                
!            if (f_mach_e_a*f_mach_e_c .lt. 0.0) then
!                mach_e_b = mach_e_c
!            else
!                mach_e_a = mach_e_c
!            endif
!                
!            if (abs(mach_e_b - mach_e_a) .ge. 0.001) then
!                goto 103
!            else
!                mach_e = mach_e_c       ! record down the final answer (which result in error +- 1%)
!                goto 102
!            endif
!            
!102         continue
!            pt_pstar = 1.0/mach_t*sqrt((k_ratio + 1.0)/(2.0 + mach_t**2*(k_ratio - 1.0)))
!            pd_pt = (1.0 + 0.5*(k_ratio - 1.0)*mach_t**2)**(k_ratio/(k_ratio - 1.0))
!            pe_pstar = 1.0/mach_e*sqrt((k_ratio + 1.0)/(2.0 + mach_e**2*(k_ratio - 1.0)))
!            pd_pe = pd_pt*pt_pstar/pe_pstar
!            if (pd_pe > press_ratio) then
!                mach_t = mach_t + 0.000001
!            else
!                error_press = abs(pd_pe - press_ratio)/press_ratio
!                if (error_press > leak_conv_criterion) then
!                    mach_t = mach_t + 0.000001
!                    m = m + 1
!                endif
!            endif
!    enddo
    ! ------------ calcualte mass flow and lambda again based on calculated mach_e ----------------
    !call leakage_lambda_mflow(t_dcv(i), p_scv(i), k_ratio, mach_e, R_specific, miu_oil, cl_rad_ro, l_com, lambda_factor, dmdtheta1_leak_rad_ro(i))  
    
    
    
    !! -----------------------------------------------------------
    !! 2 -   Vane Endface
    !! -----------------------------------------------------------
    !do i = 1, no_data + 1
    !    ! -----------------------------------
    !    ! Initialization
    !    ! -----------------------------------
    !    press_ratio = p_dcv(i)/p_scv(i)
    !    k_ratio = cp_dcv(i)/cv_dcv(i)
    !    mach_t = 0.0000001    ! initialise (Guess) mach number at throat
    !    error_press = 1.0000   ! initialise Error
    !    
    !if (press_ratio <= 1.0) then
    !    dmdtheta1_leak_vef(i) = 0.0
    !else
    !    call leakage_fanno(t_dcv(i), p_scv(i), k_ratio, mach_unity, R_specific, miu_oil, l_v(i), error_press_vef, leak_conv_criterion, press_ratio, mach_t, cl_upend, l_f_vef, dmdtheta1_leak_vef(i))
    !endif
    !! -------------------------
    !! check if program is running
    !! -------------------------
    !!if (i == no_data/2) then
    !!    print*, ' >>>>> Leakage Model is running...'
    !!endif
    !enddo
    !print*, ' >>>>> Leakage of Vane Endface Clearance ... Done'