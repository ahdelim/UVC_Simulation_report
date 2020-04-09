
    
! ---------------------------------------------
! Subroutines about the optimization algorithm 
! 1.    Random generation 
! 2.    Reflection
    
    
    
    
    
    
!subroutine optimization_random(exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, geo1, geo2, geo3, geo4 ,geo5, geo6, ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8, ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2)
subroutine optimization_random(ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8, ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2)    
    implicit none
    include "var_main_dimensions.f90"
    include "var_physical_constants.f90"
    ! ---- OPT parameters
    include "var_opt_parameters.f90"
    ! --- Geometrical variables
    double precision exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8            ! Explicit variables !  r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
    double precision exp1_2, exp2_2, exp3_2, exp4_2, exp5_2, exp6_2             ! Explicit variables ! r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv
    double precision geo1, geo2, geo3, geo4 ,geo5, geo6   ! Geometrical ! r_hc, r_ro, t_ro, l_v_ro, r_vh, e
    double precision geo1_2, geo2_2, geo3_2, geo4_2 ! Geometrical ! r_hc, l_com, e
    !common/optimization_variables/exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, geo1, geo2, geo3, geo4 ,geo5,geo 6
    ! --- Geometrical variables constraints
    double precision exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up
    double precision exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2
    double precision geo2_low, geo4_up ,geo5_up, geo6_low   ! r_hc, r_ro, t_ro, l_v_ro, r_vh
    double precision geo2_low_2, geo3_low_2, geo4_low_2   ! Geometrical constraint ! l_com, e, t_ro
    common/optimization_var_constraints/exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up, geo2_low, geo4_up ,geo5_up, geo6_low, exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2, geo2_low_2, geo3_low_2, geo4_low_2
    ! ------ optimization parameters ---
    real, dimension (1:8) :: rand
    ! ---- dummy variables
    double precision dummy_geo4, dummy_centroid
    ! ------ optimization geometrical array ---
    double precision, dimension (1:opt_ori_complex) :: ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8
    double precision, dimension (1:opt_ori_complex) :: ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2
    ! ------------------------
    ! Randomly generates explicit variables for optimization
    ! Set 1
    ! exp1 = r_oc, 2 = l_com, 3 = r_shaft, 
    ! 4 = l_vh, 5 = w_vs_ro, 6 = w_v_ro, 7 = dia_suc, 8 = dia_disc
    ! ------------------------
    ! Initialization for random seed (to generate random value between 0 to 1)
    ! ------------------------
    call RANDOM_SEED()
    call RANDOM_NUMBER(rand)        ! call an array with random number between 0 to 1
    ! ------------------------------------------------------
    ! if set 1 design variables is selected for optimization
    if (obj_var_set == 1) then      
        exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
        exp2 = exp2_low + rand(2)*(exp2_up - exp2_low)  ! l_com
        exp3 = exp3_low + rand(3)*(exp3_up - exp3_low)  ! r_shaft
        exp4 = exp4_low + rand(4)*(exp4_up - exp4_low)  ! l_vh
        exp5 = exp5_low + rand(5)*(exp5_up - exp5_low)  ! w_vs_ro
        exp6 = exp6_low + rand(6)*(exp6_up - exp6_low)  ! w_v_ro
        exp7 = exp7_low + rand(7)*(exp7_up - exp7_low)  ! dia_suc
        exp8 = exp8_low + rand(8)*(exp8_up - exp8_low)  ! dia_disc
        ! ------ Assign explicit variables to the array first ---
        ori_exp1(opt_i) = exp1
        ori_exp2(opt_i) = exp2
        ori_exp3(opt_i) = exp3
        ori_exp4(opt_i) = exp4
        ori_exp5(opt_i) = exp5
        ori_exp6(opt_i) = exp6
        ori_exp7(opt_i) = exp7
        ori_exp8(opt_i) = exp8
        ! ------ Geometrical constraints assign and check --- 
        geo1 = exp1 + 45.0  ! r_hc

        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))    ! r_ro
        geo3 = exp4 + 3.0   ! t_ro = l_vh + 1.0
        geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
        do while (geo2 < geo2_low .or. geo6 < geo6_low .or. exp3 > (geo2 - geo3 - geo6))
            if (geo2 < geo2_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2 violated, geo2 = ',geo2, ' geo2_low = ', geo2_low
            endif
            if (geo6 < geo6_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo6 violated, geo6 = ',geo6, ' geo6_low = ', geo6_low
            endif
            if (exp3 > (geo2 - geo3 - geo6)) then
                print'(1x,A)', ' Random: r_shaft is too big, violated geometrical constraint.'
            endif
                
            !dummy_centroid = (sum(ori_exp1) - exp1)/(opt_i - 1)
            !exp1 = 0.5*(exp1 + dummy_centroid)
            !if (exp1 > exp1_up .or. exp1 < exp1_low) then
            call RANDOM_SEED()
            call RANDOM_NUMBER(rand) 
            exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
            !endif
            ori_exp1(opt_i) = exp1
            print'(1x,A,F15.4)', ' New r_oc = ', exp1
            geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
            geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            if (geo2 < geo2_low .or. geo6 < geo6_low) then
                !dummy_centroid = (sum(ori_exp2) - exp2)/(opt_i - 1)
                !exp2 = 0.5*(exp2 + dummy_centroid)
                !if (exp2 > exp2_up .or. exp2 < exp2_low) then
                call RANDOM_SEED()
                call RANDOM_NUMBER(rand) 
                exp2 = exp2_low + rand(2)*(exp2_up - exp2_low)  ! r_oc
                !endif
                print'(1x,A,F10.4)', ' New l_com = ', l_com
                ori_exp2(opt_i) = exp2
                geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
                geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            endif
            
            if (exp3 > (geo2 - geo3 - geo6)) then
                call RANDOM_SEED()
                call RANDOM_NUMBER(rand) 
                !exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
                !ori_exp1(opt_i) = exp1
                !exp2 = exp2_low + rand(2)*(exp2_up - exp2_low)  ! r_oc
                !ori_exp2(opt_i) = exp2
                print'(1x,A,F10.4)', ' r_shaft is too big, current r_shaft = ',exp3
                exp3 = exp3_low + rand(3)*(exp3_up - exp3_low) ! r_shaft
                print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3
                ori_exp3(opt_i) = exp3
            endif
        enddo
    
        !do while (geo6 < geo6_low)
        !    print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo6 violated, geo6 = ',geo6, ' geo6_low = ', geo6_low
        !    dummy_centroid = (sum(ori_exp1) - exp1)/(opt_i - 1)
        !    exp1 = 0.5*(exp1 + dummy_centroid)
        !    if (exp1 > exp1_up .or. exp1 < exp1_low) then
        !        call RANDOM_SEED()
        !        call RANDOM_NUMBER(rand) 
        !        exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
        !    endif
        !    ori_exp1(opt_i) = exp1
        !    print'(1x,A,F15.4)', ' New r_oc = ', exp1
        !    geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    do while (geo2 < geo2_low)
        !        dummy_centroid = (sum(ori_exp2) - exp2)/(opt_i - 1)
        !        exp2 = 0.5*(exp2 + dummy_centroid)
        !        if (exp2 > exp2_up .or. exp2 < exp2_low) then
        !            call RANDOM_SEED()
        !            call RANDOM_NUMBER(rand) 
        !            exp2 = exp2_low + rand(1)*(exp2_up - exp2_low)  ! r_oc
        !        endif
        !        ori_exp2(opt_i) = exp2
        !        print'(1x,A,F10.4)', ' New l_com = ', l_com
        !        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    enddo
        !    geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
        !enddo
    
        !do while (exp3 > (geo2 - geo3 - geo6))
        !    call RANDOM_SEED()
        !    call RANDOM_NUMBER(rand) 
        !    exp3 = exp3_low + rand(3)*(exp3_up - exp3_low) ! r_shaft
        !    print'(1x,A,F10.4)',' r_shaft is too big, moving r_shaft to new r_shaft = ', exp3
        !    ori_exp3(opt_i) = exp3
        !enddo
    
        dummy_geo4 = 2.*exp1 - 2.*geo2  ! dummy variables for geo4 l_v_max = 2r_oc - 2r_ro (max exposed length vane)
        geo4 = dummy_geo4 + 3.0      ! l_v_ro = l_v_max + 2.0
    
        geo5 = 0.5*geo4 + 2.5   ! r_vh
    
        !do while (geo5 > geo5_up)
        !    print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo5 violated, geo5 = ',geo5, ' geo5_up = ', geo5_up
        !    dummy_centroid = (sum(ori_exp1) - exp1)/(opt_i - 1)
        !    exp1 = 0.5*(exp1 + dummy_centroid)
        !    if (exp1 > exp1_up .or. exp1 < exp1_low) then
        !        call RANDOM_SEED()
        !        call RANDOM_NUMBER(rand) 
        !        exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
        !    endif
        !    ori_exp1(opt_i) = exp1
        !    print'(1x,A,F15.4)', ' New r_oc = ', exp1
        !    geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    do while (geo2 < geo2_low)
        !        dummy_centroid = (sum(ori_exp2) - exp2)/(opt_i - 1)
        !        exp2 = 0.5*(exp2 + dummy_centroid)
        !        if (exp2 > exp2_up .or. exp2 < exp2_low) then
        !            call RANDOM_SEED()
        !            call RANDOM_NUMBER(rand) 
        !            exp2 = exp2_low + rand(1)*(exp2_up - exp2_low)  ! r_oc
        !        endif
        !        ori_exp2(opt_i) = exp2
        !        print'(1x,A,F15.4)', ' New l_com = ', l_com
        !        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    enddo
        !    dummy_geo4 = 2.*exp1 - 2.*geo2
        !    geo4 = dummy_geo4 + 3.0 
        !    geo5 = 0.5*geo4 + 2.5
        !enddo
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1 ! Outer cylinder (stator) inner radius  [mm]
        l_com = exp2    ! Compressor height/length ! [mm]
        r_shaft = exp3  ! shaft radius [mm]
        l_vh = exp4     ! vane length from the vane head/housing [mm]
        w_vs_ro = exp5  ! vane width extended from the vane head/housing (there are two small vane extended from vh, this is dimension for one)
        w_v_ro = exp6   ! vane width of the vane extended from the rotor
        dia_suc = exp7  ! suction port diameter [mm]
        dia_disc = exp8 ! discharge port diameter [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        r_ro = sqrt(r_oc**2 - vol_max/(pi*l_com)) ! Rotor outer radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor  
        t_ro = l_vh + 3.0 ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
        
        ! ---- assign to array
        ori_exp1(opt_i) = exp1
        ori_exp2(opt_i) = exp2
        ori_exp3(opt_i) = exp3
        ori_exp4(opt_i) = exp4
        ori_exp5(opt_i) = exp5
        ori_exp6(opt_i) = exp6
        ori_exp7(opt_i) = exp7
        ori_exp8(opt_i) = exp8
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77       ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
    else        ! if set 2 is selected
        exp1_2 = exp1_low_2 + rand(1)*(exp1_up_2 - exp1_low_2)  ! r_oc
        exp2_2 = exp2_low_2 + rand(2)*(exp2_up_2 - exp2_low_2)  ! r_ro
        exp3_2 = exp3_low_2 + rand(3)*(exp3_up_2 - exp3_low_2)  ! r_shaft
        exp4_2 = exp4_low_2 + rand(4)*(exp4_up_2 - exp4_low_2)  ! l_bearing
        exp5_2 = exp5_low_2 + rand(5)*(exp5_up_2 - exp5_low_2)  ! dia_disc
        exp6_2 = exp6_low_2 + rand(6)*(exp6_up_2 - exp6_low_2)  ! t_dv
        
        ! ------ Assign explicit variables to the array first ---
        ori_exp1_2(opt_i) = exp1_2    ! r_oc
        ori_exp2_2(opt_i) = exp2_2  ! r_ro
        ori_exp3_2(opt_i) = exp3_2  ! r_shaft
        ori_exp4_2(opt_i) = exp4_2  ! l_bearing
        ori_exp5_2(opt_i) = exp5_2  ! dia_disc
        ori_exp6_2(opt_i) = exp6_2 ! t_dv
        
        ! ---------------------------------------
        ! Geometrical variables calculation
        ! ---------------------------------------
        geo1_2 = exp1_2 + 45.0  ! r_hc (set 2)
        geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
        geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
        geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
        do while (geo2_2 < geo2_low_2 .or. geo3_2 < geo3_low_2 .or. geo4_2 < geo4_low_2)  ! if l_com constraints is violated
            call RANDOM_SEED()
            call RANDOM_NUMBER(rand) 
            if (geo2_2 < geo2_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2_2 violated, geo2_2 = ',geo2_2, ' geo2_low_2 = ', geo2_low_2
            endif
            if (geo3_2 < geo3_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo3_2 violated, geo3_2 = ',geo3_2, ' geo3_low_2 = ', geo3_low_2
            endif
            if (geo4_2 < (exp2_2 - geo3_2 - exp3_2)) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo4_2 violated, geo4_2 = ',geo4_2, ' geo4_low_2 = ', geo4_low_2
            endif
            print'(1x,A)', ' Randomly generates exp1_2, exp2_2 again ' 
            exp1_2 = exp1_low_2 + rand(1)*(exp1_up_2 - exp1_low_2)  ! r_oc
            exp2_2 = exp2_low_2 + rand(2)*(exp2_up_2 - exp2_low_2)  ! r_ro
            ori_exp1_2(opt_i) = exp1_2    ! r_oc, reassign array
            ori_exp2_2(opt_i) = exp2_2  ! r_ro, reassign array
            print'(1x,A,F15.4)', ' New r_oc = ', exp1_2
            print'(1x,A,F15.4)', ' New r_ro = ', exp2_2
            geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
            geo3_2 = exp1_2 - exp2_2
            geo4_2 = exp2_2 - geo3_2 - exp3_2
        enddo
        
        !do while (exp3_2 > (exp2_2 - t_ro - geo3_2))
        !    print*, exp2_2, t_ro, geo3_2
        !    pause
        !    call RANDOM_SEED()
        !    call RANDOM_NUMBER(rand) 
        !    print'(1x,A,F15.4)', ' Geometrical constraint = ', (exp2_2 - t_ro - geo3_2)
        !    print'(1x,A,F15.4)', ' current r_shaft = ', exp3_2
        !    exp3_2 = exp3_low_2 + rand(3)*(exp3_up_2 - exp3_low_2)  ! r_shaft
        !    print'(1x,A,F15.4)', ' New generated r_shaft = ', exp3_2
        !    ori_exp3_2(opt_i) = exp3_2  ! r_shaft
        !enddo
        
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1_2 ! Outer cylinder (stator) inner radius  [mm]
        r_ro = exp2_2    ! Compressor height/length ! [mm]
        r_shaft = exp3_2  ! shaft radius [mm]
        l_bearing = exp4_2     ! vane length from the vane head/housing [mm]
        dia_disc = exp5_2 ! discharge port diameter [mm]
        t_dv = exp6_2   ! reed valve thickness [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor  
        l_com = vol_max_2/(pi*(r_oc**2 - r_ro**2)) ! compressor length [mm]
        t_ro = r_ro - e - r_shaft ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
        
        ! ---- assign to array
        ori_exp1_2(opt_i) = r_oc    ! r_oc
        ori_exp2_2(opt_i) = r_ro  ! r_ro
        ori_exp3_2(opt_i) = r_shaft  ! r_shaft
        ori_exp4_2(opt_i) = l_bearing  ! l_bearing
        ori_exp5_2(opt_i) = dia_disc  ! dia_disc
        ori_exp6_2(opt_i) = t_dv ! t_dv
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77        ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 

    endif
    !write (6,*) " -------------------------------------------------------------------- "
    print*, ' '
    print*, ' '
    print*, ' '
    print*, ' '
    write (6,*) " -------------------------------------------------------------------- "
    print '(2x,A,I3)', ' Original Complex Number ',opt_i
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Main Dimensions of Compressor  "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Housing Chamber Radius         r_hc       = ", r_hc, " mm"
    write (6,2989) "Shaft Radius                   r_shaft    = ", r_shaft, " mm"
    write (6,2989) "Outer Cylinder Radius          r_oc       = ", r_oc, " mm"
    write (6,2989) "Rotor Outer Radius             r_ro       = ", r_ro, " mm"
    write (6,2989) "Rotor Inner Radius             r_roi      = ", r_roi, " mm"
    write (6,2989) "Vane Head Radius               r_vh       = ", r_vh, " mm"
    write (6,2989) "Rotor Thickness                t_ro       = ", t_ro, " mm"
    write (6,2989) "Outer Cylinder Thickness       t_oc       = ", t_oc, " mm"
    write (6,2989) "Eccentricity                   e          = ", e, " mm"
    write (6,2989) "Length of compressor           l_com      = ", l_com, " mm"
    write (6,2989) "Length of vane at V.housing    l_vh       = ", l_vh, " mm"
    write (6,2989) "Length of vane at Rotor        l_v_ro     = ", l_v_ro, " mm"
    write (6,2989) "Length of vane (max)           l_v_max    = ", l_v_max," mm"
    write (6,2989) "Length of upper bearing        l_bearing  = ", l_bearing, " mm"
    write (6,2989) "Width of vane at V.housing     w_vs_ro    = ", w_vs_ro, " mm"
    write (6,2989) "Width of vane at Rotor         w_v_ro     = ", w_v_ro, " mm"
    write (6,2989) "Width of vane (max)            w_v_max    = ", w_v_max," mm"
    write (6,2989) "Width of vane slot gap         w_vsro_gap = ", w_vsro_gap, " mm"
    write (6,2989) "Length of vane slot gap        l_vsro_gap = ", l_vsro_gap, " mm"
    write (6,*) ' '
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*)    " Port Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Suction Port Diameter             d_suc             = ", dia_suc, " mm"
    write (6,2989) "Discharge Port Diameter           d_disc            = ", dia_disc, " mm"
    write (6,2989) "Suction Port Length               length_suc        = ", length_suc, " mm"
    write (6,2989) "Discharge Port Length             length_disc       = ", length_disc, " mm"
    write (6,2989) "Suction Port start angle          theta_suc_start   = ", theta_suc_start*180.0/pi, " degree"
    write (6,2989) "Discharge Port close angle        theta_disc_start  = ", theta_disc_start*180.0/pi, " degree"
    write (6,2989) "Suction Port fully open angle     theta_suc_end     = ", theta_suc_end*180.0/pi, " degree"
    write (6,2989) "Discharge Port fully close angle  theta_disc_end    = ", theta_disc_end*180.0/pi, " degree"
    write (6,*)
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Discharge Valve Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Total Length of Discharge Valve        l_dv         = ", l_dv, " mm"
    write (6,2989) "Eff. Length of Discharge Valve         l_dv_ef      = ", l_dv_ef, " mm"
    write (6,2989) "Length of Discharge Valve Part x1                   = ", 2*r_dv_x1 - 0.77, " mm"
    write (6,2989) "Diameter of Discharge Valve Part x1    2*r_dv_x1    = ", 2*r_dv_x1, " mm"
    write (6,2989) "Length of Discharge Valve Part x2                   = ", l_dv_x3 - l_dv_x2, " mm"
    write (6,2989) "Width of Discharge Valve Part x2       w_dv_x2      = ", w_dv_x2, " mm"
    write (6,2989) "Length of Discharge Valve Part x3                   = ", l_dv_ef - l_dv_x3, " mm"
    write (6,2989) "Width of Discharge Valve Part x3       w_dv         = ", w_dv, " mm"
    write (6,2989) "Thickness of Discharge Valve           t_dv         = ", t_dv, " mm"
    
    !write (6,2989) "Distance from Fixed end to Port    x_hole_start_1  = ", x_hole_start_1, " mm"
    write (6,2989) "Maximum Deflection                     y_stop       = ", y_stop, " mm"
    
2989 format (2x,A,F8.4,A)
end subroutine optimization_random
    
    
    
subroutine optimization_reflection(ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8, ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2, best_index, worst_index, best, worst, array_obj_func)
    implicit none
    include "var_main_dimensions.f90"
    include "var_physical_constants.f90"
    integer i, ref_check
    ! ---- OPT parameters
    ! ----- optimization parameters ----- !
    include "var_opt_parameters.f90"
    ! --- Geometrical variables
    double precision exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8            ! Explicit variables !  r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
    double precision exp1_2, exp2_2, exp3_2, exp4_2, exp5_2, exp6_2             ! Explicit variables ! r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv
    double precision geo1, geo2, geo3, geo4 ,geo5, geo6   ! Geometrical ! r_hc, r_ro, t_ro, l_v_ro, r_vh, e
    double precision geo1_2, geo2_2, geo3_2, geo4_2   ! Geometrical ! r_hc, l_com, e
    !common/optimization_variables/exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, geo1, geo2, geo3, geo4 ,geo5,geo 6
    ! --- Geometrical variables constraints
    double precision exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up
    double precision exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2
    double precision geo2_low, geo4_up ,geo5_up, geo6_low   ! r_hc, r_ro, t_ro, l_v_ro, r_vh
    double precision geo2_low_2, geo3_low_2, geo4_low_2   ! Geometrical ! r_hc, l_com, e
    common/optimization_var_constraints/exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up, geo2_low, geo4_up ,geo5_up, geo6_low, exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2, geo2_low_2, geo3_low_2, geo4_low_2
    ! ------ optimization parameters ---
    real, dimension (1:8) :: rand
    ! ---- dummy variables
    double precision dummy_geo4, dummy_centroid, c_exp1, c_exp2, c_exp3, c_exp4, c_exp5, c_exp6, c_exp7, c_exp8, best, worst, s_exp
    ! ------ optimization geometrical array ---
    double precision, dimension (1:opt_ori_complex) :: ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8
    double precision, dimension (1:opt_ori_complex) :: ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2
    ! ------ objective function array
    double precision, dimension (1:opt_ori_complex) :: array_obj_func
    integer, dimension (1:1) :: best_index, worst_index
    
    ! -----------------------------------------------
    ! Reflection body
    ! 1.    Calculate centroid
    ! 2.    Reflection
    ! 3.    Move in by a factor if violate boundary
    ! 4.    Check geometrical and implicit constraints
    ! ------------------------------------------

    !print'(1x,A,F15.4)', ' Reflection factor alpha  = ', factor_a
    !print'(1x,A,F15.4)', ' Boundary factor beta     = ', factor_b
    ! --- initialization ---
    ref_check = 0
    c_exp1 = 0.d0
    c_exp2 = 0.d0
    c_exp3 = 0.d0
    c_exp4 = 0.d0
    c_exp5 = 0.d0
    c_exp6 = 0.d0
    c_exp7 = 0.d0
    c_exp8 = 0.d0
    s_exp = 0.d0        ! "Weightage"
    ! ------- dummy calculation for reflection centroid
    if (obj_var_set == 1) then
        do i = 1, opt_ori_complex
            c_exp1 = c_exp1 + ori_exp1(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp2 = c_exp2 + ori_exp2(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp3 = c_exp3 + ori_exp3(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp4 = c_exp4 + ori_exp4(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp5 = c_exp5 + ori_exp5(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp6 = c_exp6 + ori_exp6(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp7 = c_exp7 + ori_exp7(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp8 = c_exp8 + ori_exp8(i)*(array_obj_func(i) - worst)/(best - worst)
            s_exp = s_exp + (array_obj_func(i) - worst)/(best - worst)
        enddo
        ! ---- K.T. Ooi version reflection centroid
        !c_exp1 = c_exp1/(opt_ori_complex - 1)   !/s_exp
        !c_exp2 = c_exp2/(opt_ori_complex - 1)   !/s_exp
        !c_exp3 = c_exp3/(opt_ori_complex - 1)   !/s_exp
        !c_exp4 = c_exp4/(opt_ori_complex - 1)   !/s_exp
        !c_exp5 = c_exp5/(opt_ori_complex - 1)   !/s_exp
        !c_exp6 = c_exp6/(opt_ori_complex - 1)   !/s_exp
        !c_exp7 = c_exp7/(opt_ori_complex - 1)   !/s_exp
        !c_exp8 = c_exp8/(opt_ori_complex - 1)   !/s_exp
        ! ---- Y.D. Lim version reflection centroid
        c_exp1 = c_exp1/s_exp
        c_exp2 = c_exp2/s_exp
        c_exp3 = c_exp3/s_exp
        c_exp4 = c_exp4/s_exp
        c_exp5 = c_exp5/s_exp
        c_exp6 = c_exp6/s_exp
        c_exp7 = c_exp7/s_exp
        c_exp8 = c_exp8/s_exp
        
        ! ---- Reflect the explicit variables
        exp1 = (1.0 + factor_a)*c_exp1 - factor_a*ori_exp1(worst_index(1))  ! r_oc
        exp2 = (1.0 + factor_a)*c_exp2 - factor_a*ori_exp2(worst_index(1))  ! l_com
        exp3 = (1.0 + factor_a)*c_exp3 - factor_a*ori_exp3(worst_index(1))  ! r_shaft
        exp4 = (1.0 + factor_a)*c_exp4 - factor_a*ori_exp4(worst_index(1))  ! l_vh
        exp5 = (1.0 + factor_a)*c_exp5 - factor_a*ori_exp5(worst_index(1))  ! w_vs_ro
        exp6 = (1.0 + factor_a)*c_exp6 - factor_a*ori_exp6(worst_index(1))  ! w_v_ro
        exp7 = (1.0 + factor_a)*c_exp7 - factor_a*ori_exp7(worst_index(1))  ! dia_suc
        exp8 = (1.0 + factor_a)*c_exp8 - factor_a*ori_exp8(worst_index(1))  ! dia_disc
        
        ! ---- Check contraints if violate
        call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
        call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
        call optimization_exp_constraint(exp4, exp4_up, exp4_low, factor_b)
        call optimization_exp_constraint(exp5, exp5_up, exp5_low, factor_b)
        call optimization_exp_constraint(exp6, exp6_up, exp6_low, factor_b)
        call optimization_exp_constraint(exp7, exp7_up, exp7_low, factor_b)
        call optimization_exp_constraint(exp8, exp8_up, exp8_low, factor_b)
        
        ! ------ Assign explicit variables to the array first ---
        ori_exp1(worst_index(1)) = exp1
        ori_exp2(worst_index(1)) = exp2
        ori_exp3(worst_index(1)) = exp3
        ori_exp4(worst_index(1)) = exp4
        ori_exp5(worst_index(1)) = exp5
        ori_exp6(worst_index(1)) = exp6
        ori_exp7(worst_index(1)) = exp7
        ori_exp8(worst_index(1)) = exp8
        
        ! ------ Geometrical constraints assign and check --- 
        geo1 = exp1 + 45.0  ! r_hc
    
        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))    ! r_ro
        geo3 = exp4 + 3.0   ! t_ro
        geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
        do while (geo2 < geo2_low .or. geo6 < geo6_low .or. exp3 > (geo2 - geo3 - geo6))  ! if either one of the geometrical constraint violated, all geometrical constraints here are affected by the same explicit variables
            if (geo2 < geo2_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2 violated, geo2 = ',geo2, ' geo2_low = ', geo2_low
            endif
            if (geo6 < geo6_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo6 violated, geo6 = ',geo6, ' geo6_low = ', geo6_low
            endif
            if (exp3 > (geo2 - geo3 - geo6)) then
                print'(1x,A)', ' Reflect: r_shaft is too big, violated geometrical constraint.'
            endif
            
            dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
            exp1 = 0.5*(exp1 + dummy_centroid)
            call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
            ori_exp1(worst_index(1)) = exp1
            print'(1x,A,F15.4)', ' New r_oc = ', exp1
            geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))    ! r_ro
            geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            if (geo2 < geo2_low .or. geo6 < geo6_low) then
                dummy_centroid = (sum(ori_exp2) - exp2)/(opt_ori_complex - 1)
                exp2 = 0.5*(exp2 + dummy_centroid)
                call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
                ori_exp2(worst_index(1)) = exp2
                print'(1x,A,F15.4)', ' New l_com = ', l_com
                geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
                geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            endif
            
            if (exp3 > (geo2 - geo3 - geo6)) then
                print'(1x,A,F10.4)', ' Reflected r_shaft is too big = ',exp3
                dummy_centroid = (sum(ori_exp3) - exp3)/(opt_ori_complex - 1)
                exp3 = 0.5*(exp3 + dummy_centroid) ! r_shaft
                call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
                print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3
                ori_exp3(worst_index(1)) = exp3
                
                if ((ref_check >= 500) .and. (exp3 < 1.005*dummy_centroid .or. exp3 > 0.995*dummy_centroid)) then
                    call RANDOM_SEED()
                    call RANDOM_NUMBER(rand) 
                    !exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
                    !ori_exp1(opt_i) = exp1
                    !exp2 = exp2_low + rand(2)*(exp2_up - exp2_low)  ! r_oc
                    !ori_exp2(opt_i) = exp2
                    print'(1x,A,F10.4)', ' Iteration idle, randomly generate a r_shaft'
                    exp3 = exp3_low + rand(3)*(exp3_up - exp3_low) ! r_shaft
                    print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3
                    ori_exp3(worst_index(1)) = exp3
                    ref_check = 0
                endif
            endif
            ref_check = ref_check + 1
        enddo
    
        
        !do while (geo6 < geo6_low)
        !    print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo6 violated, geo6 = ',geo6, ' geo6_low = ', geo6_low
        !    dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
        !    exp1 = 0.5*(exp1 + dummy_centroid)
        !    call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        !    ori_exp1(worst_index(1)) = exp1
        !    print'(1x,A,F15.4)', ' New r_oc = ', exp1
        !    geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    do while (geo2 < geo2_low)
        !        dummy_centroid = (sum(ori_exp2) - exp2)/(opt_ori_complex - 1)
        !        exp2 = 0.5*(exp2 + dummy_centroid)
        !        call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
        !        ori_exp2(worst_index(1)) = exp2
        !        print'(1x,A,F15.4)', ' New l_com = ', l_com
        !        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    enddo
        !    geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
        !enddo
    
        !do while (exp3 > (geo2 - geo3 - geo6))      ! r_shaft = r_ro - t_ro - e
        !    !print'(1x,A)',' r_shaft is too big, moving r_shaft...'
        !    dummy_centroid = (sum(ori_exp3) - exp3)/(opt_ori_complex - 1)
        !    exp3 = 0.5*(exp3 + dummy_centroid)
        !    call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
        !    ori_exp3(worst_index(1)) = exp3
        !    if (exp3 > geo2 - geo3 - geo6) then
        !       exp3 = geo2 - geo3 - geo6
        !    endif
        !    call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
        !    print'(1x,A,F10.4)',' r_shaft is too big, moving r_shaft to new r_shaft = ', exp3
        !enddo
    
        dummy_geo4 = 2.*exp1 - 2.*geo2  ! dummy variables for geo4 l_v_max = 2r_oc - 2r_ro (max exposed length vane)
        geo4 = dummy_geo4 + 3.0      ! l_v_ro = l_v_max + 1.0
    
        geo5 = 0.5*geo4 + 2.5   ! r_vh
        !do while (geo5 > geo5_up)
        !    print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo5 violated, geo5 = ',geo5, ' geo5_up = ', geo5_up
        !    dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
        !    exp1 = 0.5*(exp1 + dummy_centroid)
        !    call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        !    ori_exp1(worst_index(1)) = exp1
        !
        !    geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    do while (geo2 < geo2_low)
        !        dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
        !        exp1 = 0.5*(exp1 + dummy_centroid)
        !        call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        !        ori_exp1(worst_index(1)) = exp1
        !        print'(1x,A,F15.4)', ' New r_oc = ', exp1
        !        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !        if (geo2 < geo2_low) then
        !            dummy_centroid = (sum(ori_exp2) - exp2)/(opt_ori_complex - 1)
        !            exp2 = 0.5*(exp2 + dummy_centroid)
        !            call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
        !            ori_exp2(worst_index(1)) = exp2
        !            print'(1x,A,F15.4)', ' New l_com = ', l_com
        !            geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !        endif
        !    enddo
        !    dummy_geo4 = 2.*exp1 - 2.*geo2
        !    geo4 = dummy_geo4 + 3.0 
        !    geo5 = 0.5*geo4 + 2.5
        !enddo
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1 ! Outer cylinder (stator) inner radius  [mm]
        l_com = exp2    ! Compressor height/length ! [mm]
        r_shaft = exp3  ! shaft radius [mm]
        l_vh = exp4     ! vane length from the vane head/housing [mm]
        w_vs_ro = exp5  ! vane width extended from the vane head/housing (there are two small vane extended from vh, this is dimension for one)
        w_v_ro = exp6   ! vane width of the vane extended from the rotor
        dia_suc = exp7  ! suction port diameter [mm]
        dia_disc = exp8 ! discharge port diameter [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        r_ro = sqrt(r_oc**2 - vol_max/(pi*l_com)) ! Rotor outer radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor  
        t_ro = l_vh + 3.0 ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 2.5   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
        
        ! ---- assign to array
        ori_exp1(worst_index(1)) = exp1
        ori_exp2(worst_index(1)) = exp2
        ori_exp3(worst_index(1)) = exp3
        ori_exp4(worst_index(1)) = exp4
        ori_exp5(worst_index(1)) = exp5
        ori_exp6(worst_index(1)) = exp6
        ori_exp7(worst_index(1)) = exp7
        ori_exp8(worst_index(1)) = exp8
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77       ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
    else
        do i = 1, opt_ori_complex
            c_exp1 = c_exp1 + ori_exp1_2(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp2 = c_exp2 + ori_exp2_2(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp3 = c_exp3 + ori_exp3_2(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp4 = c_exp4 + ori_exp4_2(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp5 = c_exp5 + ori_exp5_2(i)*(array_obj_func(i) - worst)/(best - worst)
            c_exp6 = c_exp6 + ori_exp6_2(i)*(array_obj_func(i) - worst)/(best - worst)
            s_exp = s_exp + (array_obj_func(i) - worst)/(best - worst)
        enddo
        
        ! ---- K.T. Ooi version reflection centroid
        !c_exp1 = c_exp1/(opt_ori_complex - 1)   !/s_exp
        !c_exp2 = c_exp2/(opt_ori_complex - 1)   !/s_exp
        !c_exp3 = c_exp3/(opt_ori_complex - 1)   !/s_exp
        !c_exp4 = c_exp4/(opt_ori_complex - 1)   !/s_exp
        !c_exp5 = c_exp5/(opt_ori_complex - 1)   !/s_exp
        !c_exp6 = c_exp6/(opt_ori_complex - 1)   !/s_exp

        ! ---- Y.D. Lim version reflection centroid
        c_exp1 = c_exp1/s_exp
        c_exp2 = c_exp2/s_exp
        c_exp3 = c_exp3/s_exp
        c_exp4 = c_exp4/s_exp
        c_exp5 = c_exp5/s_exp
        c_exp6 = c_exp6/s_exp
        
        ! ---- K.T. Ooi version - Reflect the explicit variables
        !print*, c_exp1, ori_exp1_2(worst_index(1)), s_exp
        exp1_2 = (1.0 + factor_a)*c_exp1 - factor_a*ori_exp1_2(worst_index(1))  ! r_oc
        exp2_2 = (1.0 + factor_a)*c_exp2 - factor_a*ori_exp2_2(worst_index(1))  ! r_ro
        exp3_2 = (1.0 + factor_a)*c_exp3 - factor_a*ori_exp3_2(worst_index(1))  ! r_shaft
        exp4_2 = (1.0 + factor_a)*c_exp4 - factor_a*ori_exp4_2(worst_index(1))  ! l_bearing
        exp5_2 = (1.0 + factor_a)*c_exp5 - factor_a*ori_exp5_2(worst_index(1))  ! dia_disc
        exp6_2 = (1.0 + factor_a)*c_exp6 - factor_a*ori_exp6_2(worst_index(1))  ! t_dv
        !print*, exp1_2
        !pause
        
        
        ! ---- Check contraints if violate
        call optimization_exp_constraint(exp1_2, exp1_up_2, exp1_low_2, factor_b)
        call optimization_exp_constraint(exp2_2, exp2_up_2, exp2_low_2, factor_b)
        call optimization_exp_constraint(exp3_2, exp3_up_2, exp3_low_2, factor_b)
        call optimization_exp_constraint(exp4_2, exp4_up_2, exp4_low_2, factor_b)
        call optimization_exp_constraint(exp5_2, exp5_up_2, exp5_low_2, factor_b)
        call optimization_exp_constraint(exp6_2, exp6_up_2, exp6_low_2, factor_b)

        
        ! ------ Assign explicit variables to the array first ---
        ori_exp1_2(worst_index(1)) = exp1_2
        ori_exp2_2(worst_index(1)) = exp2_2
        ori_exp3_2(worst_index(1)) = exp3_2
        ori_exp4_2(worst_index(1)) = exp4_2
        ori_exp5_2(worst_index(1)) = exp5_2
        ori_exp6_2(worst_index(1)) = exp6_2
        
        ! ---------------------------------------
        ! Geometrical variables calculation
        ! ---------------------------------------
        geo1_2 = exp1_2 + 45.0  ! r_hc (set 2)
        geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
        geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
        geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
        do while (geo2_2 < geo2_low_2 .or. geo3_2 < geo3_low_2 .or. geo4_2 < geo4_low_2)  ! if l_com constraints is violated

            if (geo2_2 < geo2_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2_2 violated, geo2_2 = ',geo2_2, ' geo2_low_2 = ', geo2_low_2
            endif
            if (geo3_2 < geo3_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo3_2 violated, geo3_2 = ',geo3_2, ' geo3_low_2 = ', geo3_low_2
            endif
            if (geo4_2 < geo4_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo4_2 violated, geo4_2 = ',geo4_2, ' geo4_low_2 = ', geo4_low_2
            endif
            print'(1x,A)', ' Moving r_oc and r_ro first... ' 
            dummy_centroid = (sum(ori_exp1_2) - exp1_2)/(opt_ori_complex - 1)
            exp1_2 = 0.5*(exp1_2 + dummy_centroid)
            call optimization_exp_constraint(exp1_2, exp1_up_2, exp1_low_2, factor_b)
            ori_exp1_2(worst_index(1)) = exp1_2
            print'(1x,A,F15.4)', ' New r_oc = ', exp1_2
            
            dummy_centroid = (sum(ori_exp2_2) - exp2_2)/(opt_ori_complex - 1)
            exp2_2 = 0.5*(exp2_2 + dummy_centroid)
            call optimization_exp_constraint(exp2_2, exp2_up_2, exp2_low_2, factor_b)
            ori_exp2_2(worst_index(1)) = exp2_2
            print'(1x,A,F15.4)', ' New r_ro = ', exp2_2
            
            geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
            geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
            geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
            
            if (geo4_2 < geo4_low_2) then
                print'(1x,A)', ' Still violate t_ro, moving r_shaft ' 
                dummy_centroid = (sum(ori_exp3_2) - exp3_2)/(opt_ori_complex - 1)
                exp3_2 = 0.5*(exp3_2 + dummy_centroid)
                call optimization_exp_constraint(exp3_2, exp3_up_2, exp3_low_2, factor_b)
                ori_exp3_2(worst_index(1)) = exp3_2
                print'(1x,A,F15.4)', ' New r_shaft = ', exp3_2
                
                if ((ref_check >= 500) .and. (exp3_2 < 1.005*dummy_centroid .or. exp3_2 > 0.995*dummy_centroid)) then
                    call RANDOM_SEED()
                    call RANDOM_NUMBER(rand) 
                    print'(1x,A,F10.4)', ' Iteration idle, randomly generate a r_shaft'
                    exp3_2 = exp3_low_2 + rand(3)*(exp3_up_2 - exp3_low_2) ! r_shaft
                    print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3_2
                    ori_exp3_2(worst_index(1)) = exp3_2
                    ref_check = 0
                endif
                
                geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
                geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
                geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
            endif
            ref_check = ref_check + 1
        enddo
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1_2 ! Outer cylinder (stator) inner radius  [mm]
        r_ro = exp2_2    ! Compressor height/length ! [mm]
        r_shaft = exp3_2  ! shaft radius [mm]
        l_bearing = exp4_2     ! vane length from the vane head/housing [mm]
        dia_disc = exp5_2 ! discharge port diameter [mm]
        t_dv = exp6_2   ! reed valve thickness [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor 
        l_com = vol_max_2/(pi*(r_oc**2 - r_ro**2)) ! compressor length [mm]
        t_ro = r_ro - e - r_shaft ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
         
        ! ---- assign to array
        ori_exp1_2(worst_index(1)) = r_oc    ! r_oc
        ori_exp2_2(worst_index(1)) = r_ro  ! r_ro
        ori_exp3_2(worst_index(1)) = r_shaft  ! r_shaft
        ori_exp4_2(worst_index(1)) = l_bearing  ! l_bearing
        ori_exp5_2(worst_index(1)) = dia_disc  ! dia_disc
        ori_exp6_2(worst_index(1)) = t_dv ! t_dv
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77        ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
        
    endif
    
    
    print*, ' '
    print*, ' '
    print*, ' '
    print*, ' '
    write (6,*) " -------------------------------------------------------------------- "
    print '(2x,A,I3)', ' Reflection Iteration ',opt_re
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Main Dimensions of Compressor  "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Housing Chamber Radius         r_hc       = ", r_hc, " mm"
    write (6,2989) "Shaft Radius                   r_shaft    = ", r_shaft, " mm"
    write (6,2989) "Outer Cylinder Radius          r_oc       = ", r_oc, " mm"
    write (6,2989) "Rotor Outer Radius             r_ro       = ", r_ro, " mm"
    write (6,2989) "Rotor Inner Radius             r_roi      = ", r_roi, " mm"
    write (6,2989) "Vane Head Radius               r_vh       = ", r_vh, " mm"
    write (6,2989) "Rotor Thickness                t_ro       = ", t_ro, " mm"
    write (6,2989) "Outer Cylinder Thickness       t_oc       = ", t_oc, " mm"
    write (6,2989) "Eccentricity                   e          = ", e, " mm"
    write (6,2989) "Length of compressor           l_com      = ", l_com, " mm"
    write (6,2989) "Length of vane at V.housing    l_vh       = ", l_vh, " mm"
    write (6,2989) "Length of vane at Rotor        l_v_ro     = ", l_v_ro, " mm"
    write (6,2989) "Length of vane (max)           l_v_max    = ", l_v_max," mm"
    write (6,2989) "Length of upper bearing        l_bearing  = ", l_bearing, " mm"
    write (6,2989) "Width of vane at V.housing     w_vs_ro    = ", w_vs_ro, " mm"
    write (6,2989) "Width of vane at Rotor         w_v_ro     = ", w_v_ro, " mm"
    write (6,2989) "Width of vane (max)            w_v_max    = ", w_v_max," mm"
    write (6,2989) "Width of vane slot gap         w_vsro_gap = ", w_vsro_gap, " mm"
    write (6,2989) "Length of vane slot gap        l_vsro_gap = ", l_vsro_gap, " mm"
    write (6,*) ' '
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*)    " Port Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Suction Port Diameter             d_suc             = ", dia_suc, " mm"
    write (6,2989) "Discharge Port Diameter           d_disc            = ", dia_disc, " mm"
    write (6,2989) "Suction Port Length               length_suc        = ", length_suc, " mm"
    write (6,2989) "Discharge Port Length             length_disc       = ", length_disc, " mm"
    write (6,2989) "Suction Port start angle          theta_suc_start   = ", theta_suc_start*180.0/pi, " degree"
    write (6,2989) "Discharge Port close angle        theta_disc_start  = ", theta_disc_start*180.0/pi, " degree"
    write (6,2989) "Suction Port fully open angle     theta_suc_end     = ", theta_suc_end*180.0/pi, " degree"
    write (6,2989) "Discharge Port fully close angle  theta_disc_end    = ", theta_disc_end*180.0/pi, " degree"
    write (6,*)
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Discharge Valve Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Total Length of Discharge Valve        l_dv         = ", l_dv, " mm"
    write (6,2989) "Eff. Length of Discharge Valve         l_dv_ef      = ", l_dv_ef, " mm"
    write (6,2989) "Length of Discharge Valve Part x1                   = ", 2*r_dv_x1-0.77, " mm"
    write (6,2989) "Width of Discharge Valve Part x1       2*r_dv_x1    = ", 2*r_dv_x1, " mm"
    write (6,2989) "Length of Discharge Valve Part x2                   = ", l_dv_x3 - l_dv_x2, " mm"
    write (6,2989) "Width of Discharge Valve Part x2       w_dv_x2      = ", w_dv_x2, " mm"
    write (6,2989) "Length of Discharge Valve Part x3                   = ", l_dv_ef - l_dv_x3, " mm"
    write (6,2989) "Width of Discharge Valve Part x3       w_dv         = ", w_dv, " mm"
    write (6,2989) "Thickness of Discharge Valve           t_dv         = ", t_dv, " mm"
    
    !write (6,2989) "Distance from Fixed end to Port    x_hole_start_1  = ", x_hole_start_1, " mm"
    write (6,2989) "Maximum Deflection                     y_stop       = ", y_stop, " mm"
    
2989 format (2x,A,F8.4,A)
    endsubroutine
    

subroutine optimization_best_geo(ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8, ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2, best_index)
    implicit none
    include "var_main_dimensions.f90"
    include "var_physical_constants.f90"
    ! ---- OPT parameters
    include "var_opt_parameters.f90"
    integer, dimension (1:1) :: best_index
    ! ------ optimization geometrical array ---
    double precision, dimension (1:opt_ori_complex) :: ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8
    double precision, dimension (1:opt_ori_complex) :: ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2
    if (obj_var_set == 1) then
        ! ---- assign the best explicit variables to variables used in the simulation
        r_oc = ori_exp1(best_index(1)) ! Outer cylinder (stator) inner radius  [mm]
        l_com = ori_exp2(best_index(1))    ! Compressor height/length ! [mm]
        r_shaft = ori_exp3(best_index(1))  ! shaft radius [mm]
        l_vh = ori_exp4(best_index(1))     ! vane length from the vane head/housing [mm]
        w_vs_ro = ori_exp5(best_index(1))  ! vane width extended from the vane head/housing (there are two small vane extended from vh, this is dimension for one)
        w_v_ro = ori_exp6(best_index(1))   ! vane width of the vane extended from the rotor
        dia_suc = ori_exp7(best_index(1))  ! suction port diameter [mm]
        dia_disc = ori_exp8(best_index(1)) ! discharge port diameter [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        r_ro = sqrt(r_oc**2 - vol_max/(pi*l_com)) ! Rotor outer radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor 
        t_ro = l_vh + 3.0 ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
         

        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]

        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77       ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
        
    else
        
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = ori_exp1_2(best_index(1)) ! Outer cylinder (stator) inner radius  [mm]
        r_ro = ori_exp2_2(best_index(1))    ! Compressor height/length ! [mm]
        r_shaft = ori_exp3_2(best_index(1))  ! shaft radius [mm]
        l_bearing = ori_exp4_2(best_index(1))     ! vane length from the vane head/housing [mm]
        dia_disc = ori_exp5_2(best_index(1)) ! discharge port diameter [mm]
        t_dv = ori_exp6_2(best_index(1))   ! reed valve thickness [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor  
        l_com = vol_max_2/(pi*(r_oc**2 - r_ro**2)) ! compressor length [mm]
        t_ro = r_ro - e - r_shaft ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
        

        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77       ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
    endif
    
    write (155,*) " =================================================================== "
    print '(2x,A,I3)', ' Total Number of Iterations ',opt_re
    write (155,*) " ------------------------------------------------------------------- "
    write (155,*) " Best Design Parameters of Compressor  "
    write (155,*) " ------------------------------------------------------------------- "
    write (155,2989) "Housing Chamber Radius         r_hc       = ", r_hc, " mm"
    write (155,2989) "Shaft Radius                   r_shaft    = ", r_shaft, " mm"
    write (155,2989) "Outer Cylinder Radius          r_oc       = ", r_oc, " mm"
    write (155,2989) "Rotor Outer Radius             r_ro       = ", r_ro, " mm"
    write (155,2989) "Rotor Inner Radius             r_roi      = ", r_roi, " mm"
    write (155,2989) "Vane Head Radius               r_vh       = ", r_vh, " mm"
    write (155,2989) "Rotor Thickness                t_ro       = ", t_ro, " mm"
    write (155,2989) "Outer Cylinder Thickness       t_oc       = ", t_oc, " mm"
    write (155,2989) "Eccentricity                   e          = ", e, " mm"
    write (155,2989) "Length of compressor           l_com      = ", l_com, " mm"
    write (155,2989) "Length of vane at V.housing    l_vh       = ", l_vh, " mm"
    write (155,2989) "Length of vane at Rotor        l_v_ro     = ", l_v_ro, " mm"
    write (155,2989) "Length of vane (max)           l_v_max    = ", l_v_max," mm"
    write (155,2989) "Length of upper bearing        l_bearing  = ", l_bearing, " mm"
    write (155,2989) "Width of vane at V.housing     w_vs_ro    = ", w_vs_ro, " mm"
    write (155,2989) "Width of vane at Rotor         w_v_ro     = ", w_v_ro, " mm"
    write (155,2989) "Width of vane (max)            w_v_max    = ", w_v_max," mm"
    write (155,2989) "Width of vane slot gap         w_vsro_gap = ", w_vsro_gap, " mm"
    write (155,2989) "Length of vane slot gap        l_vsro_gap = ", l_vsro_gap, " mm"
    write (155,*) ' '
    write (155,*) " -------------------------------------------------------------------- "
    write (155,*)    " Port Dimensions "
    write (155,*) " -------------------------------------------------------------------- "
    write (155,2989) "Suction Port Diameter             d_suc             = ", dia_suc, " mm"
    write (155,2989) "Discharge Port Diameter           d_disc            = ", dia_disc, " mm"
    write (155,2989) "Suction Port Length               length_suc        = ", length_suc, " mm"
    write (155,2989) "Discharge Port Length             length_disc       = ", length_disc, " mm"
    write (155,2989) "Suction Port start angle          theta_suc_start   = ", theta_suc_start*180.0/pi, " degree"
    write (155,2989) "Discharge Port close angle        theta_disc_start  = ", theta_disc_start*180.0/pi, " degree"
    write (155,2989) "Suction Port fully open angle     theta_suc_end     = ", theta_suc_end*180.0/pi, " degree"
    write (155,2989) "Discharge Port fully close angle  theta_disc_end    = ", theta_disc_end*180.0/pi, " degree"
    write (155,*)
    write (155,*) " -------------------------------------------------------------------- "
    write (155,*) " Discharge Valve Dimensions "
    write (155,*) " -------------------------------------------------------------------- "
    write (155,2989) "Total Length of Discharge Valve        l_dv         = ", l_dv, " mm"
    write (155,2989) "Eff. Length of Discharge Valve         l_dv_ef      = ", l_dv_ef, " mm"
    write (155,2989) "Length of Discharge Valve Part x1                   = ", 2*r_dv_x1-0.77, " mm"
    write (155,2989) "Width of Discharge Valve Part x1       2*r_dv_x1    = ", 2*r_dv_x1, " mm"
    write (155,2989) "Length of Discharge Valve Part x2                   = ", l_dv_x3 - l_dv_x2, " mm"
    write (155,2989) "Width of Discharge Valve Part x2       w_dv_x2      = ", w_dv_x2, " mm"
    write (155,2989) "Length of Discharge Valve Part x3                   = ", l_dv_ef - l_dv_x3, " mm"
    write (155,2989) "Width of Discharge Valve Part x3       w_dv         = ", w_dv, " mm"
    write (155,2989) "Thickness of Discharge Valve           t_dv         = ", t_dv, " mm"
    write (155,2989) "Maximum Deflection                     y_stop       = ", y_stop, " mm"
    write (155,*)
    
2989 format (2x,A,F8.4,A)
2990 format (2x,A,f12.4,A)
endsubroutine
    
    
subroutine optimization_improv_move(ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8, ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2, best_index, worst_index, best, worst, array_obj_func)
    implicit none
    include "var_main_dimensions.f90"
    include "var_physical_constants.f90"
    integer i, ref_check
    ! ----- optimization parameters ----- !
    include "var_opt_parameters.f90"
    ! --- Geometrical variables
    double precision exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8            ! Explicit variables !  r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
    double precision exp1_2, exp2_2, exp3_2, exp4_2, exp5_2, exp6_2             ! Explicit variables ! r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv
    double precision geo1, geo2, geo3, geo4 ,geo5, geo6   ! Geometrical ! r_hc, r_ro, t_ro, l_v_ro, r_vh, e
    double precision geo1_2, geo2_2, geo3_2, geo4_2   ! Geometrical ! r_hc, l_com, e
    !common/optimization_variables/exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, geo1, geo2, geo3, geo4 ,geo5,geo 6
    ! --- Geometrical variables constraints
    double precision exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up
    double precision exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2
    double precision geo2_low, geo4_up ,geo5_up, geo6_low   ! r_hc, r_ro, t_ro, l_v_ro, r_vh
    double precision geo2_low_2, geo3_low_2, geo4_low_2   ! Geometrical ! r_hc, l_com, e
    common/optimization_var_constraints/exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up, geo2_low, geo4_up ,geo5_up, geo6_low, exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2, geo2_low_2, geo3_low_2, geo4_low_2
    ! ------ optimization parameters ---
    real, dimension (1:8) :: rand
    ! ---- dummy variables
    double precision dummy_geo4, dummy_centroid, c_exp1, c_exp2, c_exp3, c_exp4, c_exp5, c_exp6, c_exp7, c_exp8, best, worst, s_exp
    ! ------ optimization geometrical array ---
    double precision, dimension (1:opt_ori_complex) :: ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8
    double precision, dimension (1:opt_ori_complex) :: ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2
    ! ------ objective function array
    double precision, dimension (1:opt_ori_complex) :: array_obj_func
    integer, dimension (1:1) :: best_index, worst_index
    
    ! --- initialization ---
    ref_check = 0
    
    if (obj_var_set == 1) then
        ! ---- Move the explicit variables halfway to the best complex
        exp1 = 0.5*(ori_exp1(worst_index(1)) + ori_exp1(best_index(1)))  ! r_oc
        exp2 = 0.5*(ori_exp2(worst_index(1)) + ori_exp2(best_index(1)))  ! l_com
        exp3 = 0.5*(ori_exp3(worst_index(1)) + ori_exp3(best_index(1)))  ! r_shaft
        exp4 = 0.5*(ori_exp4(worst_index(1)) + ori_exp4(best_index(1)))  ! l_vh
        exp5 = 0.5*(ori_exp5(worst_index(1)) + ori_exp5(best_index(1)))  ! w_vs_ro
        exp6 = 0.5*(ori_exp6(worst_index(1)) + ori_exp6(best_index(1)))  ! w_v_ro
        exp7 = 0.5*(ori_exp7(worst_index(1)) + ori_exp7(best_index(1)))  ! dia_suc
        exp8 = 0.5*(ori_exp8(worst_index(1)) + ori_exp8(best_index(1)))  ! dia_disc
        
        ! ---- Check contraints if violate
        call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
        call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
        call optimization_exp_constraint(exp4, exp4_up, exp4_low, factor_b)
        call optimization_exp_constraint(exp5, exp5_up, exp5_low, factor_b)
        call optimization_exp_constraint(exp6, exp6_up, exp6_low, factor_b)
        call optimization_exp_constraint(exp7, exp7_up, exp7_low, factor_b)
        call optimization_exp_constraint(exp8, exp8_up, exp8_low, factor_b)
        
        ! ------ Assign explicit variables to the array first ---
        ori_exp1(worst_index(1)) = exp1
        ori_exp2(worst_index(1)) = exp2
        ori_exp3(worst_index(1)) = exp3
        ori_exp4(worst_index(1)) = exp4
        ori_exp5(worst_index(1)) = exp5
        ori_exp6(worst_index(1)) = exp6
        ori_exp7(worst_index(1)) = exp7
        ori_exp8(worst_index(1)) = exp8
        
        !ori_exp1(worst_index(1)) = 0.5*(ori_exp1(worst_index(1)) + ori_exp1(best_index(1)))
        !ori_exp2(worst_index(1)) = 0.5*(ori_exp2(worst_index(1)) + ori_exp2(best_index(1)))
        !ori_exp3(worst_index(1)) = 0.5*(ori_exp3(worst_index(1)) + ori_exp3(best_index(1)))
        !ori_exp4(worst_index(1)) = 0.5*(ori_exp4(worst_index(1)) + ori_exp4(best_index(1)))
        !ori_exp5(worst_index(1)) = 0.5*(ori_exp5(worst_index(1)) + ori_exp5(best_index(1)))
        !ori_exp6(worst_index(1)) = 0.5*(ori_exp6(worst_index(1)) + ori_exp6(best_index(1)))
        !ori_exp7(worst_index(1)) = 0.5*(ori_exp7(worst_index(1)) + ori_exp7(best_index(1)))
        !ori_exp8(worst_index(1)) = 0.5*(ori_exp8(worst_index(1)) + ori_exp8(best_index(1)))
        !

        ! ------ Geometrical constraints assign and check --- 
        geo1 = exp1 + 45.0  ! r_hc
    
        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))    ! r_ro
        geo3 = exp4 + 3.0   ! t_ro
        geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
        do while (geo2 < geo2_low .or. geo6 < geo6_low .or. exp3 > (geo2 - geo3 - geo6))  ! if either one of the geometrical constraint violated, all geometrical constraints here are affected by the same explicit variables
            if (geo2 < geo2_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2 violated, geo2 = ',geo2, ' geo2_low = ', geo2_low
            endif
            if (geo6 < geo6_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo6 violated, geo6 = ',geo6, ' geo6_low = ', geo6_low
            endif
            if (exp3 > (geo2 - geo3 - geo6)) then
                print'(1x,A)', ' Reflect: r_shaft is too big, violated geometrical constraint.'
            endif
            
            dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
            exp1 = 0.5*(exp1 + dummy_centroid)
            call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
            ori_exp1(worst_index(1)) = exp1
            print'(1x,A,F15.4)', ' New r_oc = ', exp1
            geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))    ! r_ro
            geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            if (geo2 < geo2_low .or. geo6 < geo6_low) then
                dummy_centroid = (sum(ori_exp2) - exp2)/(opt_ori_complex - 1)
                exp2 = 0.5*(exp2 + dummy_centroid)
                call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
                ori_exp2(worst_index(1)) = exp2
                print'(1x,A,F15.4)', ' New l_com = ', l_com
                geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
                geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            endif
            
            if (exp3 > (geo2 - geo3 - geo6)) then
                print'(1x,A,F10.4)', ' Reflected r_shaft is too big = ',exp3
                dummy_centroid = (sum(ori_exp3) - exp3)/(opt_ori_complex - 1)
                exp3 = 0.5*(exp3 + dummy_centroid) ! r_shaft
                call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
                print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3
                ori_exp3(worst_index(1)) = exp3
                
                if ((ref_check >= 500) .and. (exp3 < 1.005*dummy_centroid .or. exp3 > 0.995*dummy_centroid)) then
                    call RANDOM_SEED()
                    call RANDOM_NUMBER(rand) 
                    !exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
                    !ori_exp1(opt_i) = exp1
                    !exp2 = exp2_low + rand(2)*(exp2_up - exp2_low)  ! r_oc
                    !ori_exp2(opt_i) = exp2
                    print'(1x,A,F10.4)', ' Iteration idle, randomly generate a r_shaft'
                    exp3 = exp3_low + rand(3)*(exp3_up - exp3_low) ! r_shaft
                    print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3
                    ori_exp3(worst_index(1)) = exp3
                    ref_check = 0
                endif
            endif
            ref_check = ref_check + 1
        enddo
    
        dummy_geo4 = 2.*exp1 - 2.*geo2  ! dummy variables for geo4 l_v_max = 2r_oc - 2r_ro (max exposed length vane)
        geo4 = dummy_geo4 + 3.0      ! l_v_ro = l_v_max + 1.0
    
        geo5 = 0.5*geo4 + 2.5   ! r_vh
        !do while (geo5 > geo5_up)
        !    print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo5 violated, geo5 = ',geo5, ' geo5_up = ', geo5_up
        !    dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
        !    exp1 = 0.5*(exp1 + dummy_centroid)
        !    call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        !    ori_exp1(worst_index(1)) = exp1
        !
        !    geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !    do while (geo2 < geo2_low)
        !        dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
        !        exp1 = 0.5*(exp1 + dummy_centroid)
        !        call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        !        ori_exp1(worst_index(1)) = exp1
        !        print'(1x,A,F15.4)', ' New r_oc = ', exp1
        !        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !        if (geo2 < geo2_low) then
        !            dummy_centroid = (sum(ori_exp2) - exp2)/(opt_ori_complex - 1)
        !            exp2 = 0.5*(exp2 + dummy_centroid)
        !            call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
        !            ori_exp2(worst_index(1)) = exp2
        !            print'(1x,A,F15.4)', ' New l_com = ', l_com
        !            geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
        !        endif
        !    enddo
        !    dummy_geo4 = 2.*exp1 - 2.*geo2
        !    geo4 = dummy_geo4 + 3.0 
        !    geo5 = 0.5*geo4 + 2.5
        !enddo
        
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1 ! Outer cylinder (stator) inner radius  [mm]
        l_com = exp2    ! Compressor height/length ! [mm]
        r_shaft = exp3  ! shaft radius [mm]
        l_vh = exp4     ! vane length from the vane head/housing [mm]
        w_vs_ro = exp5  ! vane width extended from the vane head/housing (there are two small vane extended from vh, this is dimension for one)
        w_v_ro = exp6   ! vane width of the vane extended from the rotor
        dia_suc = exp7  ! suction port diameter [mm]
        dia_disc = exp8 ! discharge port diameter [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        r_ro = sqrt(r_oc**2 - vol_max/(pi*l_com)) ! Rotor outer radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor
        t_ro = l_vh + 3.0 ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 2.5   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
          
        ! ---- assign to array
        ori_exp1(worst_index(1)) = exp1
        ori_exp2(worst_index(1)) = exp2
        ori_exp3(worst_index(1)) = exp3
        ori_exp4(worst_index(1)) = exp4
        ori_exp5(worst_index(1)) = exp5
        ori_exp6(worst_index(1)) = exp6
        ori_exp7(worst_index(1)) = exp7
        ori_exp8(worst_index(1)) = exp8
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77       ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
    else
        ! ----- Move the worst explicit variables halfway to the best explicit variables
        exp1_2 = 0.5*(ori_exp1_2(worst_index(1)) + ori_exp1_2(best_index(1)))  ! r_oc
        exp2_2 = 0.5*(ori_exp2_2(worst_index(1)) + ori_exp2_2(best_index(1)))  ! r_ro
        exp3_2 = 0.5*(ori_exp3_2(worst_index(1)) + ori_exp3_2(best_index(1)))  ! r_shaft
        exp4_2 = 0.5*(ori_exp4_2(worst_index(1)) + ori_exp4_2(best_index(1)))  ! l_bearing
        exp5_2 = 0.5*(ori_exp5_2(worst_index(1)) + ori_exp5_2(best_index(1)))  ! dia_disc
        exp6_2 = 0.5*(ori_exp6_2(worst_index(1)) + ori_exp6_2(best_index(1)))  ! t_dv
        
        !print'(1x,4F10.4)', ori_exp1_2(worst_index(1)), ori_exp1_2(best_index(1)), exp1_2      ! for testing value (monitoring)
        
        ! ---- Check contraints if violate
        call optimization_exp_constraint(exp1_2, exp1_up_2, exp1_low_2, factor_b)
        call optimization_exp_constraint(exp2_2, exp2_up_2, exp2_low_2, factor_b)
        call optimization_exp_constraint(exp3_2, exp3_up_2, exp3_low_2, factor_b)
        call optimization_exp_constraint(exp4_2, exp4_up_2, exp4_low_2, factor_b)
        call optimization_exp_constraint(exp5_2, exp5_up_2, exp5_low_2, factor_b)
        call optimization_exp_constraint(exp6_2, exp6_up_2, exp6_low_2, factor_b)

        
        ! ------ Assign explicit variables to the array first ---
        ori_exp1_2(worst_index(1)) = exp1_2
        ori_exp2_2(worst_index(1)) = exp2_2
        ori_exp3_2(worst_index(1)) = exp3_2
        ori_exp4_2(worst_index(1)) = exp4_2
        ori_exp5_2(worst_index(1)) = exp5_2
        ori_exp6_2(worst_index(1)) = exp6_2
        
        ! ---------------------------------------
        ! Geometrical variables calculation
        ! ---------------------------------------
        geo1_2 = exp1_2 + 45.0  ! r_hc (set 2)
        geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
        geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
        geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
        do while (geo2_2 < geo2_low_2 .or. geo3_2 < geo3_low_2 .or. geo4_2 < geo4_low_2)  ! if l_com constraints is violated

            if (geo2_2 < geo2_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2_2 violated, geo2_2 = ',geo2_2, ' geo2_low_2 = ', geo2_low_2
            endif
            if (geo3_2 < geo3_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo3_2 violated, geo3_2 = ',geo3_2, ' geo3_low_2 = ', geo3_low_2
            endif
            if (geo4_2 < geo4_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo4_2 violated, geo4_2 = ',geo4_2, ' geo4_low_2 = ', geo4_low_2
            endif
            print'(1x,A)', ' Moving r_oc and r_ro first... ' 
            dummy_centroid = (sum(ori_exp1_2) - exp1_2)/(opt_ori_complex - 1)
            exp1_2 = 0.5*(exp1_2 + dummy_centroid)
            call optimization_exp_constraint(exp1_2, exp1_up_2, exp1_low_2, factor_b)
            ori_exp1_2(worst_index(1)) = exp1_2
            print'(1x,A,F15.4)', ' New r_oc = ', exp1_2
            
            dummy_centroid = (sum(ori_exp2_2) - exp2_2)/(opt_ori_complex - 1)
            exp2_2 = 0.5*(exp2_2 + dummy_centroid)
            call optimization_exp_constraint(exp2_2, exp2_up_2, exp2_low_2, factor_b)
            ori_exp2_2(worst_index(1)) = exp2_2
            print'(1x,A,F15.4)', ' New r_ro = ', exp2_2
            
            geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
            geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
            geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
            
            if (geo4_2 < geo4_low_2) then
                print'(1x,A)', ' Still violate t_ro, moving r_shaft ' 
                dummy_centroid = (sum(ori_exp3_2) - exp3_2)/(opt_ori_complex - 1)
                exp3_2 = 0.5*(exp3_2 + dummy_centroid)
                call optimization_exp_constraint(exp3_2, exp3_up_2, exp3_low_2, factor_b)
                ori_exp3_2(worst_index(1)) = exp3_2
                print'(1x,A,F15.4)', ' New r_shaft = ', exp3_2
                
                if ((ref_check >= 500) .and. (exp3_2 < 1.005*dummy_centroid .or. exp3_2 > 0.995*dummy_centroid)) then
                    call RANDOM_SEED()
                    call RANDOM_NUMBER(rand) 
                    print'(1x,A,F10.4)', ' Iteration idle, randomly generate a r_shaft'
                    exp3_2 = exp3_low_2 + rand(3)*(exp3_up_2 - exp3_low_2) ! r_shaft
                    print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3_2
                    ori_exp3_2(worst_index(1)) = exp3_2
                    ref_check = 0
                endif
                
                geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
                geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
                geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
            endif
            ref_check = ref_check + 1
        enddo
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1_2 ! Outer cylinder (stator) inner radius  [mm]
        r_ro = exp2_2    ! Compressor height/length ! [mm]
        r_shaft = exp3_2  ! shaft radius [mm]
        l_bearing = exp4_2     ! vane length from the vane head/housing [mm]
        dia_disc = exp5_2 ! discharge port diameter [mm]
        t_dv = exp6_2   ! reed valve thickness [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor 
        l_com = vol_max_2/(pi*(r_oc**2 - r_ro**2)) ! compressor length [mm]
        t_ro = r_ro - e - r_shaft ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
        ! ---- assign to array
        ori_exp1_2(worst_index(1)) = r_oc    ! r_oc
        ori_exp2_2(worst_index(1)) = r_ro  ! r_ro
        ori_exp3_2(worst_index(1)) = r_shaft  ! r_shaft
        ori_exp4_2(worst_index(1)) = l_bearing  ! l_bearing
        ori_exp5_2(worst_index(1)) = dia_disc  ! dia_disc
        ori_exp6_2(worst_index(1)) = t_dv ! t_dv
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77       ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
        
    endif
    
    print*, ' '
    print*, ' '
    print*, ' '
    print*, ' '
    write (6,*) " =================================================================== "
    print '(2x,A,I3)', ' Best Index Number = ', best_index(1)
    print '(2x,A,I3)', ' Moved Index Number = ',worst_index(1)
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Main Dimensions of Compressor  "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Housing Chamber Radius         r_hc       = ", r_hc, " mm"
    write (6,2989) "Shaft Radius                   r_shaft    = ", r_shaft, " mm"
    write (6,2989) "Outer Cylinder Radius          r_oc       = ", r_oc, " mm"
    write (6,2989) "Rotor Outer Radius             r_ro       = ", r_ro, " mm"
    write (6,2989) "Rotor Inner Radius             r_roi      = ", r_roi, " mm"
    write (6,2989) "Vane Head Radius               r_vh       = ", r_vh, " mm"
    write (6,2989) "Rotor Thickness                t_ro       = ", t_ro, " mm"
    write (6,2989) "Outer Cylinder Thickness       t_oc       = ", t_oc, " mm"
    write (6,2989) "Eccentricity                   e          = ", e, " mm"
    write (6,2989) "Length of compressor           l_com      = ", l_com, " mm"
    write (6,2989) "Length of vane at V.housing    l_vh       = ", l_vh, " mm"
    write (6,2989) "Length of vane at Rotor        l_v_ro     = ", l_v_ro, " mm"
    write (6,2989) "Length of vane (max)           l_v_max    = ", l_v_max," mm"
    write (6,2989) "Length of upper bearing        l_bearing  = ", l_bearing, " mm"
    write (6,2989) "Width of vane at V.housing     w_vs_ro    = ", w_vs_ro, " mm"
    write (6,2989) "Width of vane at Rotor         w_v_ro     = ", w_v_ro, " mm"
    write (6,2989) "Width of vane (max)            w_v_max    = ", w_v_max," mm"
    write (6,2989) "Width of vane slot gap         w_vsro_gap = ", w_vsro_gap, " mm"
    write (6,2989) "Length of vane slot gap        l_vsro_gap = ", l_vsro_gap, " mm"
    write (6,*) ' '
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*)    " Port Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Suction Port Diameter             d_suc             = ", dia_suc, " mm"
    write (6,2989) "Discharge Port Diameter           d_disc            = ", dia_disc, " mm"
    write (6,2989) "Suction Port Length               length_suc        = ", length_suc, " mm"
    write (6,2989) "Discharge Port Length             length_disc       = ", length_disc, " mm"
    write (6,2989) "Suction Port start angle          theta_suc_start   = ", theta_suc_start*180.0/pi, " degree"
    write (6,2989) "Discharge Port close angle        theta_disc_start  = ", theta_disc_start*180.0/pi, " degree"
    write (6,2989) "Suction Port fully open angle     theta_suc_end     = ", theta_suc_end*180.0/pi, " degree"
    write (6,2989) "Discharge Port fully close angle  theta_disc_end    = ", theta_disc_end*180.0/pi, " degree"
    write (6,*)
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Discharge Valve Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Total Length of Discharge Valve        l_dv         = ", l_dv, " mm"
    write (6,2989) "Eff. Length of Discharge Valve         l_dv_ef      = ", l_dv_ef, " mm"
    write (6,2989) "Length of Discharge Valve Part x1                   = ", 2*r_dv_x1-0.77, " mm"
    write (6,2989) "Width of Discharge Valve Part x1       2*r_dv_x1    = ", 2*r_dv_x1, " mm"
    write (6,2989) "Length of Discharge Valve Part x2                   = ", l_dv_x3 - l_dv_x2, " mm"
    write (6,2989) "Width of Discharge Valve Part x2       w_dv_x2      = ", w_dv_x2, " mm"
    write (6,2989) "Length of Discharge Valve Part x3                   = ", l_dv_ef - l_dv_x3, " mm"
    write (6,2989) "Width of Discharge Valve Part x3       w_dv         = ", w_dv, " mm"
    write (6,2989) "Thickness of Discharge Valve           t_dv         = ", t_dv, " mm"
    
    !write (6,2989) "Distance from Fixed end to Port    x_hole_start_1  = ", x_hole_start_1, " mm"
    write (6,2989) "Maximum Deflection                     y_stop       = ", y_stop, " mm"
    write (6,*)
    
2989 format (2x,A,F8.4,A)
2990 format (2x,A,f12.4,A)
endsubroutine optimization_improv_move
    
    
subroutine optimization_force_move(ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8, ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2, best_index, worst_index, best, worst, array_obj_func)
    implicit none
    include "var_main_dimensions.f90"
    include "var_physical_constants.f90"
    integer i, ref_check
    ! ----- optimization parameters ----- !
    include "var_opt_parameters.f90" 
    ! --- Geometrical variables
    double precision exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8            ! Explicit variables !  r_oc, l_com, r_shaft, l_vh, w_vs_ro, w_v_ro, dia_suc, dia_disc
    double precision exp1_2, exp2_2, exp3_2, exp4_2, exp5_2, exp6_2             ! Explicit variables ! r_oc, r_ro, r_shaft, l_bearing, dia_disc, t_dv
    double precision geo1, geo2, geo3, geo4 ,geo5, geo6   ! Geometrical ! r_hc, r_ro, t_ro, l_v_ro, r_vh, e
    double precision geo1_2, geo2_2, geo3_2, geo4_2   ! Geometrical ! r_hc, l_com, e
    !common/optimization_variables/exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, geo1, geo2, geo3, geo4 ,geo5,geo 6
    ! --- Geometrical variables constraints
    double precision exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up
    double precision exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2
    double precision geo2_low, geo4_up ,geo5_up, geo6_low   ! r_hc, r_ro, t_ro, l_v_ro, r_vh
    double precision geo2_low_2, geo3_low_2, geo4_low_2   ! Geometrical ! r_hc, l_com, e
    common/optimization_var_constraints/exp1_low, exp1_up, exp2_low, exp2_up, exp3_low, exp3_up, exp4_low, exp4_up, exp5_low, exp5_up, exp6_low, exp6_up, exp7_low, exp7_up, exp8_low, exp8_up, geo2_low, geo4_up ,geo5_up, geo6_low, exp1_low_2, exp1_up_2, exp2_low_2, exp2_up_2, exp3_low_2, exp3_up_2, exp4_low_2, exp4_up_2, exp5_low_2, exp5_up_2, exp6_low_2, exp6_up_2, geo2_low_2, geo3_low_2, geo4_low_2
    ! ------ optimization parameters ---
    real, dimension (1:8) :: rand
    ! ---- dummy variables
    double precision dummy_geo4, dummy_centroid, c_exp1, c_exp2, c_exp3, c_exp4, c_exp5, c_exp6, c_exp7, c_exp8, best, worst, s_exp
    ! ------ optimization geometrical array ---
    double precision, dimension (1:opt_ori_complex) :: ori_exp1, ori_exp2, ori_exp3, ori_exp4, ori_exp5, ori_exp6, ori_exp7, ori_exp8
    double precision, dimension (1:opt_ori_complex) :: ori_exp1_2, ori_exp2_2, ori_exp3_2, ori_exp4_2, ori_exp5_2, ori_exp6_2
    ! ------ objective function array
    double precision, dimension (1:opt_ori_complex) :: array_obj_func
    integer, dimension (1:1) :: best_index, worst_index
    
    ! --- initialization ---
    ref_check = 0
    
    if (obj_var_set == 1) then
        ! ---- Move the explicit variables to the best complex
        exp1 = ori_exp1(best_index(1))  !   0.5*(ori_exp1(worst_index(1)) + ori_exp1(best_index(1)))  ! r_oc
        exp2 = ori_exp2(best_index(1))  !   0.5*(ori_exp2(worst_index(1)) + ori_exp2(best_index(1)))  ! l_com
        exp3 = ori_exp3(best_index(1))  !!  0.5*(ori_exp3(worst_index(1)) + ori_exp3(best_index(1)))  ! r_shaft
        exp4 = ori_exp4(best_index(1))  !   0.5*(ori_exp4(worst_index(1)) + ori_exp4(best_index(1)))  ! l_vh
        exp5 = ori_exp5(best_index(1))  !   0.5*(ori_exp5(worst_index(1)) + ori_exp5(best_index(1)))  ! w_vs_ro
        exp6 = ori_exp6(best_index(1))  !   0.5*(ori_exp6(worst_index(1)) + ori_exp6(best_index(1)))  ! w_v_ro
        exp7 = ori_exp7(best_index(1))  !   0.5*(ori_exp7(worst_index(1)) + ori_exp7(best_index(1)))  ! dia_suc
        exp8 = ori_exp8(best_index(1))  !   0.5*(ori_exp8(worst_index(1)) + ori_exp8(best_index(1)))  ! dia_disc
        
        ! ---- Check contraints if violate
        call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
        call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
        call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
        call optimization_exp_constraint(exp4, exp4_up, exp4_low, factor_b)
        call optimization_exp_constraint(exp5, exp5_up, exp5_low, factor_b)
        call optimization_exp_constraint(exp6, exp6_up, exp6_low, factor_b)
        call optimization_exp_constraint(exp7, exp7_up, exp7_low, factor_b)
        call optimization_exp_constraint(exp8, exp8_up, exp8_low, factor_b)
        
        ! ------ Assign explicit variables to the array first ---
        ori_exp1(worst_index(1)) = exp1
        ori_exp2(worst_index(1)) = exp2
        ori_exp3(worst_index(1)) = exp3
        ori_exp4(worst_index(1)) = exp4
        ori_exp5(worst_index(1)) = exp5
        ori_exp6(worst_index(1)) = exp6
        ori_exp7(worst_index(1)) = exp7
        ori_exp8(worst_index(1)) = exp8
        
        !ori_exp1(worst_index(1)) = 0.5*(ori_exp1(worst_index(1)) + ori_exp1(best_index(1)))
        !ori_exp2(worst_index(1)) = 0.5*(ori_exp2(worst_index(1)) + ori_exp2(best_index(1)))
        !ori_exp3(worst_index(1)) = 0.5*(ori_exp3(worst_index(1)) + ori_exp3(best_index(1)))
        !ori_exp4(worst_index(1)) = 0.5*(ori_exp4(worst_index(1)) + ori_exp4(best_index(1)))
        !ori_exp5(worst_index(1)) = 0.5*(ori_exp5(worst_index(1)) + ori_exp5(best_index(1)))
        !ori_exp6(worst_index(1)) = 0.5*(ori_exp6(worst_index(1)) + ori_exp6(best_index(1)))
        !ori_exp7(worst_index(1)) = 0.5*(ori_exp7(worst_index(1)) + ori_exp7(best_index(1)))
        !ori_exp8(worst_index(1)) = 0.5*(ori_exp8(worst_index(1)) + ori_exp8(best_index(1)))
        !

        ! ------ Geometrical constraints assign and check --- 
        geo1 = exp1 + 45.0  ! r_hc
    
        geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))    ! r_ro
        geo3 = exp4 + 3.0   ! t_ro
        geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
        do while (geo2 < geo2_low .or. geo6 < geo6_low .or. exp3 > (geo2 - geo3 - geo6))  ! if either one of the geometrical constraint violated, all geometrical constraints here are affected by the same explicit variables
            if (geo2 < geo2_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2 violated, geo2 = ',geo2, ' geo2_low = ', geo2_low
            endif
            if (geo6 < geo6_low) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo6 violated, geo6 = ',geo6, ' geo6_low = ', geo6_low
            endif
            if (exp3 > (geo2 - geo3 - geo6)) then
                print'(1x,A)', ' Reflect: r_shaft is too big, violated geometrical constraint.'
            endif
            
            dummy_centroid = (sum(ori_exp1) - exp1)/(opt_ori_complex - 1)
            exp1 = 0.5*(exp1 + dummy_centroid)
            call optimization_exp_constraint(exp1, exp1_up, exp1_low, factor_b)
            ori_exp1(worst_index(1)) = exp1
            print'(1x,A,F15.4)', ' New r_oc = ', exp1
            geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))    ! r_ro
            geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            if (geo2 < geo2_low .or. geo6 < geo6_low) then
                dummy_centroid = (sum(ori_exp2) - exp2)/(opt_ori_complex - 1)
                exp2 = 0.5*(exp2 + dummy_centroid)
                call optimization_exp_constraint(exp2, exp2_up, exp2_low, factor_b)
                ori_exp2(worst_index(1)) = exp2
                print'(1x,A,F15.4)', ' New l_com = ', l_com
                geo2 = sqrt(exp1**2 - vol_max/(pi*exp2))
                geo6 = exp1 - geo2   ! eccentric e = r_oc - r_ro
            endif
            
            if (exp3 > (geo2 - geo3 - geo6)) then
                print'(1x,A,F10.4)', ' Reflected r_shaft is too big = ',exp3
                dummy_centroid = (sum(ori_exp3) - exp3)/(opt_ori_complex - 1)
                exp3 = 0.5*(exp3 + dummy_centroid) ! r_shaft
                call optimization_exp_constraint(exp3, exp3_up, exp3_low, factor_b)
                print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3
                ori_exp3(worst_index(1)) = exp3
                
                if ((ref_check >= 500) .and. (exp3 < 1.005*dummy_centroid .or. exp3 > 0.995*dummy_centroid)) then
                    call RANDOM_SEED()
                    call RANDOM_NUMBER(rand) 
                    !exp1 = exp1_low + rand(1)*(exp1_up - exp1_low)  ! r_oc
                    !ori_exp1(opt_i) = exp1
                    !exp2 = exp2_low + rand(2)*(exp2_up - exp2_low)  ! r_oc
                    !ori_exp2(opt_i) = exp2
                    print'(1x,A,F10.4)', ' Iteration idle, randomly generate a r_shaft'
                    exp3 = exp3_low + rand(3)*(exp3_up - exp3_low) ! r_shaft
                    print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3
                    ori_exp3(worst_index(1)) = exp3
                    ref_check = 0
                endif
            endif
            ref_check = ref_check + 1
        enddo
    
        dummy_geo4 = 2.*exp1 - 2.*geo2  ! dummy variables for geo4 l_v_max = 2r_oc - 2r_ro (max exposed length vane)
        geo4 = dummy_geo4 + 3.0      ! l_v_ro = l_v_max + 1.0
    
        geo5 = 0.5*geo4 + 2.5   ! r_vh
        
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1 ! Outer cylinder (stator) inner radius  [mm]
        l_com = exp2    ! Compressor height/length ! [mm]
        r_shaft = exp3  ! shaft radius [mm]
        l_vh = exp4     ! vane length from the vane head/housing [mm]
        w_vs_ro = exp5  ! vane width extended from the vane head/housing (there are two small vane extended from vh, this is dimension for one)
        w_v_ro = exp6   ! vane width of the vane extended from the rotor
        dia_suc = exp7  ! suction port diameter [mm]
        dia_disc = exp8 ! discharge port diameter [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        r_ro = sqrt(r_oc**2 - vol_max/(pi*l_com)) ! Rotor outer radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor 
        t_ro = l_vh + 3.0 ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 2.5   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
         
        ! ---- assign to array
        ori_exp1(worst_index(1)) = exp1
        ori_exp2(worst_index(1)) = exp2
        ori_exp3(worst_index(1)) = exp3
        ori_exp4(worst_index(1)) = exp4
        ori_exp5(worst_index(1)) = exp5
        ori_exp6(worst_index(1)) = exp6
        ori_exp7(worst_index(1)) = exp7
        ori_exp8(worst_index(1)) = exp8
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77        ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
    else
        ! ----- Move the worst explicit variables to the best explicit variables
        exp1_2 = ori_exp1_2(best_index(1))  !0.5*(ori_exp1_2(worst_index(1)) + ori_exp1_2(best_index(1)))  ! r_oc
        exp2_2 = ori_exp2_2(best_index(1))  !0.5*(ori_exp2_2(worst_index(1)) + ori_exp2_2(best_index(1)))  ! r_ro
        exp3_2 = ori_exp3_2(best_index(1))  !0.5*(ori_exp3_2(worst_index(1)) + ori_exp3_2(best_index(1)))  ! r_shaft
        exp4_2 = ori_exp4_2(best_index(1))  !0.5*(ori_exp4_2(worst_index(1)) + ori_exp4_2(best_index(1)))  ! l_bearing
        exp5_2 = ori_exp5_2(best_index(1))  !0.5*(ori_exp5_2(worst_index(1)) + ori_exp5_2(best_index(1)))  ! dia_disc
        exp6_2 = ori_exp6_2(best_index(1))  !0.5*(ori_exp6_2(worst_index(1)) + ori_exp6_2(best_index(1)))  ! t_dv
        
        !print'(1x,4F10.4)', ori_exp1_2(worst_index(1)), ori_exp1_2(best_index(1)), exp1_2      ! for testing value (monitoring)
        
        ! ---- Check contraints if violate
        call optimization_exp_constraint(exp1_2, exp1_up_2, exp1_low_2, factor_b)
        call optimization_exp_constraint(exp2_2, exp2_up_2, exp2_low_2, factor_b)
        call optimization_exp_constraint(exp3_2, exp3_up_2, exp3_low_2, factor_b)
        call optimization_exp_constraint(exp4_2, exp4_up_2, exp4_low_2, factor_b)
        call optimization_exp_constraint(exp5_2, exp5_up_2, exp5_low_2, factor_b)
        call optimization_exp_constraint(exp6_2, exp6_up_2, exp6_low_2, factor_b)

        
        ! ------ Assign explicit variables to the array first ---
        ori_exp1_2(worst_index(1)) = exp1_2
        ori_exp2_2(worst_index(1)) = exp2_2
        ori_exp3_2(worst_index(1)) = exp3_2
        ori_exp4_2(worst_index(1)) = exp4_2
        ori_exp5_2(worst_index(1)) = exp5_2
        ori_exp6_2(worst_index(1)) = exp6_2
        
        ! ---------------------------------------
        ! Geometrical variables calculation
        ! ---------------------------------------
        geo1_2 = exp1_2 + 45.0  ! r_hc (set 2)
        geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
        geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
        geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
        do while (geo2_2 < geo2_low_2 .or. geo3_2 < geo3_low_2 .or. geo4_2 < geo4_low_2)  ! if l_com constraints is violated

            if (geo2_2 < geo2_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2_2 violated, geo2_2 = ',geo2_2, ' geo2_low_2 = ', geo2_low_2
            endif
            if (geo3_2 < geo3_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo3_2 violated, geo3_2 = ',geo3_2, ' geo3_low_2 = ', geo3_low_2
            endif
            if (geo4_2 < geo4_low_2) then
                print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo4_2 violated, geo4_2 = ',geo4_2, ' geo4_low_2 = ', geo4_low_2
            endif
            print'(1x,A)', ' Moving r_oc and r_ro first... ' 
            dummy_centroid = (sum(ori_exp1_2) - exp1_2)/(opt_ori_complex - 1)
            exp1_2 = 0.5*(exp1_2 + dummy_centroid)
            call optimization_exp_constraint(exp1_2, exp1_up_2, exp1_low_2, factor_b)
            ori_exp1_2(worst_index(1)) = exp1_2
            print'(1x,A,F15.4)', ' New r_oc = ', exp1_2
            
            dummy_centroid = (sum(ori_exp2_2) - exp2_2)/(opt_ori_complex - 1)
            exp2_2 = 0.5*(exp2_2 + dummy_centroid)
            call optimization_exp_constraint(exp2_2, exp2_up_2, exp2_low_2, factor_b)
            ori_exp2_2(worst_index(1)) = exp2_2
            print'(1x,A,F15.4)', ' New r_ro = ', exp2_2
            
            geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
            geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
            geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
            
            if (geo4_2 < geo4_low_2) then
                print'(1x,A)', ' Still violate t_ro, moving r_shaft ' 
                dummy_centroid = (sum(ori_exp3_2) - exp3_2)/(opt_ori_complex - 1)
                exp3_2 = 0.5*(exp3_2 + dummy_centroid)
                call optimization_exp_constraint(exp3_2, exp3_up_2, exp3_low_2, factor_b)
                ori_exp3_2(worst_index(1)) = exp3_2
                print'(1x,A,F15.4)', ' New r_shaft = ', exp3_2
                
                if ((ref_check >= 500) .and. (exp3_2 < 1.005*dummy_centroid .or. exp3_2 > 0.995*dummy_centroid)) then
                    call RANDOM_SEED()
                    call RANDOM_NUMBER(rand) 
                    print'(1x,A,F10.4)', ' Iteration idle, randomly generate a r_shaft'
                    exp3_2 = exp3_low_2 + rand(3)*(exp3_up_2 - exp3_low_2) ! r_shaft
                    print'(1x,A,F10.4)',' moving r_shaft to new r_shaft = ', exp3_2
                    ori_exp3_2(worst_index(1)) = exp3_2
                    ref_check = 0
                endif
                
                geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
                geo3_2 = exp1_2 - exp2_2        ! e = r_oc - r_ro
                geo4_2 = exp2_2 - geo3_2 - exp3_2   ! t_ro = r_ro - e - r_shaft
            endif
            ref_check = ref_check + 1
        enddo
        ! ---- assign the explicit variables to variables used in the simulation
        r_oc = exp1_2 ! Outer cylinder (stator) inner radius  [mm]
        r_ro = exp2_2    ! Compressor height/length ! [mm]
        r_shaft = exp3_2  ! shaft radius [mm]
        l_bearing = exp4_2     ! vane length from the vane head/housing [mm]
        dia_disc = exp5_2 ! discharge port diameter [mm]
        t_dv = exp6_2   ! reed valve thickness [mm]
        ! ---- assign geometrical constraints to simulation variables
        r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
        e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor  
        l_com = vol_max_2/(pi*(r_oc**2 - r_ro**2)) ! compressor length [mm]
        t_ro = r_ro - e - r_shaft ! Rotor thickness [mm]
        l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
        l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
        r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
        
        ! ---- assign to array
        ori_exp1_2(worst_index(1)) = r_oc    ! r_oc
        ori_exp2_2(worst_index(1)) = r_ro  ! r_ro
        ori_exp3_2(worst_index(1)) = r_shaft  ! r_shaft
        ori_exp4_2(worst_index(1)) = l_bearing  ! l_bearing
        ori_exp5_2(worst_index(1)) = dia_disc  ! dia_disc
        ori_exp6_2(worst_index(1)) = t_dv ! t_dv
        ! Other dimensions 
        r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
        l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
        w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
        r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
        r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    
        ! Dimension of Roller (attached to shaft as one whole piece)
        r_ecc = r_roi      ! Roller radius 
        l_ecc = l_com      ! roller length
    
    
        theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
        theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
        theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
        theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
    
    
        ! ------------------------------------------------------
        ! Circular reed valve illustration
        ! ------------------------------------------------------
    
        ! Reed valve used in NVC is a circular valve
        ! _______          ________
        ! |      |________/        \
        ! |                         |
        ! |______|--------\________/
        ! part 3.. part 2 .. part 1..
        ! screw..  neck ...  port 
        !                   <------- x = 0 (start from the free end)
    
        ! -------------------------------------------------------
        ! -------------------------------------------------------
    
        ! Dimensions of Discharge Valve
        l_dv = 45.25
        l_dv_ef = l_dv - 8.0
        l_dv_neck = 26.45
        r_dv_x1 = 6.25 !0.5*dia_disc + 1.25 ! part 1, circular part radius
        l_dv_x2 = 2.*r_dv_x1 - 0.77       ! the position, x value where part 2 start
        l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
        
    endif
    
    print*, ' '
    print*, ' '
    print*, ' '
    print*, ' '
    write (6,*) " =================================================================== "
    print '(2x,A,I3)', ' Best Index Number = ', best_index(1)
    print '(2x,A,I3)', ' Moved Index Number = ',worst_index(1)
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Main Dimensions of Compressor  "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Housing Chamber Radius         r_hc       = ", r_hc, " mm"
    write (6,2989) "Shaft Radius                   r_shaft    = ", r_shaft, " mm"
    write (6,2989) "Outer Cylinder Radius          r_oc       = ", r_oc, " mm"
    write (6,2989) "Rotor Outer Radius             r_ro       = ", r_ro, " mm"
    write (6,2989) "Rotor Inner Radius             r_roi      = ", r_roi, " mm"
    write (6,2989) "Vane Head Radius               r_vh       = ", r_vh, " mm"
    write (6,2989) "Rotor Thickness                t_ro       = ", t_ro, " mm"
    write (6,2989) "Outer Cylinder Thickness       t_oc       = ", t_oc, " mm"
    write (6,2989) "Eccentricity                   e          = ", e, " mm"
    write (6,2989) "Length of compressor           l_com      = ", l_com, " mm"
    write (6,2989) "Length of vane at V.housing    l_vh       = ", l_vh, " mm"
    write (6,2989) "Length of vane at Rotor        l_v_ro     = ", l_v_ro, " mm"
    write (6,2989) "Length of vane (max)           l_v_max    = ", l_v_max," mm"
    write (6,2989) "Length of upper bearing        l_bearing  = ", l_bearing, " mm"
    write (6,2989) "Width of vane at V.housing     w_vs_ro    = ", w_vs_ro, " mm"
    write (6,2989) "Width of vane at Rotor         w_v_ro     = ", w_v_ro, " mm"
    write (6,2989) "Width of vane (max)            w_v_max    = ", w_v_max," mm"
    write (6,2989) "Width of vane slot gap         w_vsro_gap = ", w_vsro_gap, " mm"
    write (6,2989) "Length of vane slot gap        l_vsro_gap = ", l_vsro_gap, " mm"
    write (6,*) ' '
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*)    " Port Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Suction Port Diameter             d_suc             = ", dia_suc, " mm"
    write (6,2989) "Discharge Port Diameter           d_disc            = ", dia_disc, " mm"
    write (6,2989) "Suction Port Length               length_suc        = ", length_suc, " mm"
    write (6,2989) "Discharge Port Length             length_disc       = ", length_disc, " mm"
    write (6,2989) "Suction Port start angle          theta_suc_start   = ", theta_suc_start*180.0/pi, " degree"
    write (6,2989) "Discharge Port close angle        theta_disc_start  = ", theta_disc_start*180.0/pi, " degree"
    write (6,2989) "Suction Port fully open angle     theta_suc_end     = ", theta_suc_end*180.0/pi, " degree"
    write (6,2989) "Discharge Port fully close angle  theta_disc_end    = ", theta_disc_end*180.0/pi, " degree"
    write (6,*)
    write (6,*) " -------------------------------------------------------------------- "
    write (6,*) " Discharge Valve Dimensions "
    write (6,*) " -------------------------------------------------------------------- "
    write (6,2989) "Total Length of Discharge Valve        l_dv         = ", l_dv, " mm"
    write (6,2989) "Eff. Length of Discharge Valve         l_dv_ef      = ", l_dv_ef, " mm"
    write (6,2989) "Length of Discharge Valve Part x1                   = ", 2*r_dv_x1 - 0.77, " mm"
    write (6,2989) "Diameter of Discharge Valve Part x1    2*r_dv_x1    = ", 2*r_dv_x1, " mm"
    write (6,2989) "Length of Discharge Valve Part x2                   = ", l_dv_x3 - l_dv_x2, " mm"
    write (6,2989) "Width of Discharge Valve Part x2       w_dv_x2      = ", w_dv_x2, " mm"
    write (6,2989) "Length of Discharge Valve Part x3                   = ", l_dv_ef - l_dv_x3, " mm"
    write (6,2989) "Width of Discharge Valve Part x3       w_dv         = ", w_dv, " mm"
    write (6,2989) "Thickness of Discharge Valve           t_dv         = ", t_dv, " mm"
    
    !write (6,2989) "Distance from Fixed end to Port    x_hole_start_1  = ", x_hole_start_1, " mm"
    write (6,2989) "Maximum Deflection                     y_stop       = ", y_stop, " mm"
    write (6,*)
    
2989 format (2x,A,F8.4,A)
2990 format (2x,A,f12.4,A)
endsubroutine optimization_force_move