!subroutine GA_geo_constraint_check(r_oc, ratio_ro_cyl, r_shaft)
!    double precision r_oc, ratio_ro_cyl, r_shaft        ! main variables
!    double precision r_ro, e, l_com, t_ro
!    double precision, parameter :: vol_max_GA = 34.0, pi = 3.14159265359 ! [cc]
!    double precision, parameter :: e_low = 0.5, l_com_low = 21.0, t_ro_low = 8.0, r_ro_low = 21.0
!    ! ---------------------------------------
!    ! Geometrical variables constraint checking
!    ! ---------------------------------------
!    !vol_max_GA = vol_max_GA*1000.0 ! convert [cc] to [mm3]
!    r_ro = r_oc*ratio_ro_cyl    ! rotor radius = cylinder radius * rotor-cylinder ratio
!    
!    l_com = vol_max_GA/(pi*(r_oc**2 - r_ro**2)) ! l_com = max_vol/pi(cylinder radius^2 - rtoor radius^2)
!    e = r_oc - r_ro        ! e = r_oc - r_ro
!    t_ro = r_ro - e - r_shaft   ! t_ro = r_ro - e - r_shaft
!    do while (e < e_low .or. l_com < l_com_low .or. t_ro < t_ro_low)  ! if any constraints is violated
!        
!        if (e < e_low) then
!            print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo2_2 violated, geo2_2 = ',e, ' geo2_low_2 = ', e_low
!        endif
!        if (l_com < l_com_low) then
!            print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo3_2 violated, geo3_2 = ',l_com, ' geo3_low_2 = ', l_com_low
!        endif
!        if (t_ro < t_ro_low) then
!            print'(1x,A,F10.4,A,F10.4)', ' Geometrical constraints geo4_2 violated, geo4_2 = ',t_ro, ' geo4_low_2 = ', t_ro_low
!        endif
!        ! -------------------------------------
!        ! This part was commented out for you to decide whether you want to use it or not
!        ! If yes, then replace those undeclared variables to the declared variables
!        ! i.e., exp1_2 --> r_oc, exp1_low_2 --> lower constraint of r_oc
!        ! -------------------------------------
!        !call RANDOM_SEED()
!        !call RANDOM_NUMBER(rand) 
!        !print'(1x,A)', ' Randomly generates exp1_2, exp2_2 again ' 
!        !exp1_2 = exp1_low_2 + rand(1)*(exp1_up_2 - exp1_low_2)  ! r_oc
!        !exp2_2 = exp2_low_2 + rand(2)*(exp2_up_2 - exp2_low_2)  ! r_ro
!        !ori_exp1_2(opt_i) = exp1_2    ! r_oc, reassign array
!        !ori_exp2_2(opt_i) = exp2_2  ! r_ro, reassign array
!        !print'(1x,A,F15.4)', ' New r_oc = ', exp1_2
!        !print'(1x,A,F15.4)', ' New r_ro = ', exp2_2
!        !geo2_2 = vol_max_2/(pi*(exp1_2**2 - exp2_2**2)) ! l_com
!        !geo3_2 = exp1_2 - exp2_2
!        !geo4_2 = exp2_2 - geo3_2 - exp3_2
!    enddo
!    
!endsubroutine