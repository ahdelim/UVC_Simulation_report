!subroutine GA_main_dimension(var1,var2,var3,var4,var5,var6)
!    implicit none
!    include "var_main_dimension.f90"
!    include "var_physical_constant.f90"
!    double precision var1,var2,var3,var4,var5,var6  ! <--- these are your variables that have already satisfied the constraints
!    double precision vol_max_GA
!    common/GA_constraint/vol_max_GA
!    ! ---- assign the explicit variables to variables used in the simulation
!    r_oc = var1 ! Outer cylinder (stator) inner radius  [mm]
!    r_ro = var*var1    ! Compressor height/length ! [mm]
!    r_shaft = var3  ! shaft radius [mm]
!    l_bearing = var4     ! vane length from the vane head/housing [mm]
!    dia_suc = var5 ! suction port diameter [mm]
!    dia_disc = var6   ! discharge port thickness [mm]
!    ! ---- assign geometrical constraints to simulation variables
!    r_hc = r_oc + 45.0 ! Housing chamber radius [mm]
!    e = r_oc - r_ro    ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor  
!    l_com = vol_max_GA/(pi*(r_oc**2 - r_ro**2)) ! compressor length [mm]
!    t_ro = r_ro - e - r_shaft ! Rotor thickness [mm]
!    l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
!    l_v_ro = l_v_max + 3.0   ! vane length from the rotor (the middle vane) [mm]
!    r_vh = 0.5*l_v_ro + 2.5 ! Radius of the head of vane (vane head, vh) [mm]
!        
!    ! Other dimensions 
!    r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
!    l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
!    w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
!    r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
!    r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
!    
!    ! Dimension of Roller (attached to shaft as one whole piece)
!    r_ecc = r_roi      ! Roller radius 
!    l_ecc = l_com      ! roller length
!    
!    
!    theta_suc_start = theta_suc_center - atan(dia_suc/(2.*r_oc))    ! angle where suction port just open   [rad]
!    theta_suc_end = theta_suc_center + atan(dia_suc/(2.*r_oc))      ! angle where suction port fully open   [rad]
!    theta_disc_start = theta_disc_center - atan(dia_disc/(2.*r_oc)) ! angle where discharge port just about to close    [rad]
!    theta_disc_end = theta_disc_center + atan(dia_disc/(2.*r_oc))   ! angle where discharge port fully close    [rad]
!    
!    
!    ! ------------------------------------------------------
!    ! Circular reed valve illustration
!    ! ------------------------------------------------------
!    
!    ! Reed valve used in NVC is a circular valve
!    ! _______          ________
!    ! |      |________/        \
!    ! |                         |
!    ! |______|--------\________/
!    ! part 3.. part 2 .. part 1..
!    ! screw..  neck ...  port 
!    !                   <------- x = 0 (start from the free end)
!    
!    ! -------------------------------------------------------
!    ! -------------------------------------------------------
!    
!    ! Dimensions of Discharge Valve
!    l_dv = 45.25
!    l_dv_ef = l_dv - 6.0
!    l_dv_neck = 20.0
!    r_dv_x1 = 0.5*dia_disc + 0.5 ! part 1, circular part radius
!    l_dv_x2 = 2.*r_dv_x1        ! the position, x value where part 2 start
!    l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
!
!    endif
!    !write (6,*) " -------------------------------------------------------------------- "
!    print*, ' '
!    print*, ' '
!    print*, ' '
!    print*, ' '
!    !write (6,*) " -------------------------------------------------------------------- "       ! <---- Input the iteration number of easy tracking, or population number?
!    !print '(2x,A,I3)', ' Original Complex Number ',opt_i
!    write (6,*) " -------------------------------------------------------------------- "
!    write (6,*) " Main Dimensions of Compressor  "
!    write (6,*) " -------------------------------------------------------------------- "
!    write (6,2989) "Housing Chamber Radius         r_hc       = ", r_hc, " mm"
!    write (6,2989) "Shaft Radius                   r_shaft    = ", r_shaft, " mm"
!    write (6,2989) "Outer Cylinder Radius          r_oc       = ", r_oc, " mm"
!    write (6,2989) "Rotor Outer Radius             r_ro       = ", r_ro, " mm"
!    write (6,2989) "Rotor Inner Radius             r_roi      = ", r_roi, " mm"
!    write (6,2989) "Vane Head Radius               r_vh       = ", r_vh, " mm"
!    write (6,2989) "Rotor Thickness                t_ro       = ", t_ro, " mm"
!    write (6,2989) "Outer Cylinder Thickness       t_oc       = ", t_oc, " mm"
!    write (6,2989) "Eccentricity                   e          = ", e, " mm"
!    write (6,2989) "Length of compressor           l_com      = ", l_com, " mm"
!    write (6,2989) "Length of vane at V.housing    l_vh       = ", l_vh, " mm"
!    write (6,2989) "Length of vane at Rotor        l_v_ro     = ", l_v_ro, " mm"
!    write (6,2989) "Length of vane (max)           l_v_max    = ", l_v_max," mm"
!    write (6,2989) "Length of upper bearing        l_bearing  = ", l_bearing, " mm"
!    write (6,2989) "Width of vane at V.housing     w_vs_ro    = ", w_vs_ro, " mm"
!    write (6,2989) "Width of vane at Rotor         w_v_ro     = ", w_v_ro, " mm"
!    write (6,2989) "Width of vane (max)            w_v_max    = ", w_v_max," mm"
!    write (6,2989) "Width of vane slot gap         w_vsro_gap = ", w_vsro_gap, " mm"
!    write (6,2989) "Length of vane slot gap        l_vsro_gap = ", l_vsro_gap, " mm"
!    write (6,*) ' '
!    write (6,*) " -------------------------------------------------------------------- "
!    write (6,*)    " Port Dimensions "
!    write (6,*) " -------------------------------------------------------------------- "
!    write (6,2989) "Suction Port Diameter             d_suc             = ", dia_suc, " mm"
!    write (6,2989) "Discharge Port Diameter           d_disc            = ", dia_disc, " mm"
!    write (6,2989) "Suction Port Length               length_suc        = ", length_suc, " mm"
!    write (6,2989) "Discharge Port Length             length_disc       = ", length_disc, " mm"
!    write (6,2989) "Suction Port start angle          theta_suc_start   = ", theta_suc_start*180.0/pi, " degree"
!    write (6,2989) "Discharge Port close angle        theta_disc_start  = ", theta_disc_start*180.0/pi, " degree"
!    write (6,2989) "Suction Port fully open angle     theta_suc_end     = ", theta_suc_end*180.0/pi, " degree"
!    write (6,2989) "Discharge Port fully close angle  theta_disc_end    = ", theta_disc_end*180.0/pi, " degree"
!    write (6,*)
!    write (6,*) " -------------------------------------------------------------------- "
!    write (6,*) " Discharge Valve Dimensions "
!    write (6,*) " -------------------------------------------------------------------- "
!    write (6,2989) "Total Length of Discharge Valve        l_dv         = ", l_dv, " mm"
!    write (6,2989) "Eff. Length of Discharge Valve         l_dv_ef      = ", l_dv_ef, " mm"
!    write (6,2989) "Length of Discharge Valve Part x1      2*r_dv_x1    = ", 2*r_dv_x1, " mm"
!    write (6,2989) "Width of Discharge Valve Part x1       radius       = ", r_dv_x1, " mm"
!    write (6,2989) "Length of Discharge Valve Part x2                   = ", l_dv_x3 - l_dv_x2, " mm"
!    write (6,2989) "Width of Discharge Valve Part x2       w_dv_x2      = ", w_dv_x2, " mm"
!    write (6,2989) "Length of Discharge Valve Part x3                   = ", l_dv_ef - l_dv_x3, " mm"
!    write (6,2989) "Width of Discharge Valve Part x3       w_dv         = ", w_dv, " mm"
!    write (6,2989) "Thickness of Discharge Valve           t_dv         = ", t_dv, " mm"
!    
!    !write (6,2989) "Distance from Fixed end to Port    x_hole_start_1  = ", x_hole_start_1, " mm"
!    write (6,2989) "Maximum Deflection                     y_stop       = ", y_stop, " mm"
!    
!2989 format (2x,A,F8.4,A)
!     
!endsubroutine 