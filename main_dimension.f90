subroutine main_dimension
    
    ! --------------------------------------
    ! main_dimension.f90 read and calculate the dimension input from the file
    ! File name (File no. 110) = "Input Main dimensions.txt"
    ! The parameters that the variables are refering to is named after the variables in the input file
    ! --------------------------------------
    ! include the .f90 file to declare the variables
    ! --------------------------------------
    include "var_main_dimensions.f90"
    include "var_physical_constants.f90"
    double precision sur_tension_oil, angle_oil_film, ratio_val_port
    common/valve_para_3/sur_tension_oil, angle_oil_film, ratio_val_port
    ! ------------------------
    ! Dimension of body
    read(110,*) r_hc        ! Housing chamber radius [mm]
    read(110,*) r_oc        ! Outer cylinder (stator) inner radius  [mm]
    read(110,*) r_ro        ! Rotor outer radius [mm]
    read(110,*) t_ro        ! Rotor thickness [mm]
    read(110,*) t_oc        ! Outer cylinder thickness [mm]
    read(110,*) t_oc_cover  ! [mm]	Outer Cylinder Cover Thickness
    read(110,*) t_oc_base	! [mm]	Outer Cylinder Base Thickness
    ! Dimension of vane head
    read(110,*) r_vh        ! Radius of the head of vane (vane head, vh) [mm]
    
    ! Dimension of Shaft and Roller
    read(110,*) r_shaft     ! shaft radius [mm]
    
    ! Dimension of journal bearing
    r_bearing = r_shaft     ! Bearing radius, equal this to  shaft radius in simulation    [mm]
    read(110,*) l_bearing   ! Bearing Length    [mm]
    l_bearing_lower = t_oc_base + 0.5! Lower bearing length [mm]
    
    ! dimensions of vanes
    read(110,*) l_vh      ! vane length from the vane head/housing [mm]
    read(110,*) l_v_ro      ! vane length from the rotor (the middle vane) [mm] from rotor Outer diameter
    read(110,*) l_exposed_vh    ! Exposed length of vane housing in the chamber (i.e. vane housing circular part is partly exposed in chamber) [mm] 
    l_v_max = 2*r_oc - 2*r_ro     ! max. vane length exposed in the chamber, this is a must ---> l_vh + l_v_ro > l_v_max [mm]
    l_vs_ro = l_vh          ! vane slot length at the rotor [mm] (assumed almost same length)
    read(110,*) w_vs_ro      ! vane width extended from the vane head/housing (there are two small vane extended from vh, this is dimension for one)
    read(110,*) w_v_ro      ! vane width of the vane extended from the rotor
    
    w_v_max = 2*w_vs_ro + w_v_ro                 ! [mm]	Maximum Width of vane (after merged)
    r_oco = r_oc + t_oc             ! [mm] Outer cylinder outer radius
    r_roi = r_ro - t_ro                   ! [mm] Inner radius of rotor
    e = r_oc - r_ro                     ! [mm] eccentric = r_oc - r_ro, distance between the centre of stator and rotor 
    
    read(110,*) w_vsro_gap  ! [mm] width of the gap between rotor vane slot and vane housing's vane
    read(110,*) l_vsro_gap   ! [mm] length of the gap between rotor vane slot and vane housing's vane
    read(110,*) w_vs_oil_path   ! [mm] width of rotor vane slot oil path 
    read(110,*) h_vs_oil_path		! [mm] height of rotor vane slot oil path
    read(110,*) l_vs_oil_path   ! [mm] length of rotor vane slot oil path
    ! Length of compressors
    read(110,*) l_com       ! [mm]
    read(110,*) l_ecc      ! [mm]
    ! Dimension of Roller (attached to shaft as one whole piece) (eccentric)
    read(110,*) r_ecc   ! [mm]
    !r_ecc = r_roi - 0.04      ! Roller radius  ! 33.82
    !l_ecc = l_com - 10.0      ! roller length
    
    ! --------------- Port Size -------------
    read(110,*) dia_suc     ! suction port diameter [mm]
    read(110,*) dia_disc    ! discharge port diameter [mm]

    read(110,*) length_suc  ! suction port length   [mm]
    read(110,*) length_disc ! discharge port length [mm]
    
    
    ! -------------------- Port position parameter ---------------
    read(110,*) theta_suc_center    ! angle to the center line of the suction port  [degree]
    read(110,*) theta_disc_center   ! angle to the center line of the discharge port    [degree]
    
    theta_suc_center = theta_suc_center*pi/180.0        ! convert [degree] to [rad]
    theta_disc_center = theta_disc_center*pi/180.0      ! convert [degree] to [rad]
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
    read(110,*) dia_dv      ! diameter of reed valve part 1
    read(110,*) w_dv        ! reed valve width at part 3 (where the screw at)
    read(110,*) w_dv_x2     ! reed valve width of part 2 (the neck)
    read(110,*) l_dv_neck   ! length of neck
    read(110,*) t_dv        ! reed valve thickness
    read(110,*) l_dv        ! total length of reed valve (include screw end)
    read(110,*) y_stop      ! stop plate height (y_start, the free end will touch it first
    l_dv_ef = l_dv - 7.0   ! Effective length (calculated from the screw edge, because the srew lock one end (clamped)), it should calculate from the clamped part (by valve stop)
    r_dv_x1 = 0.5*dia_dv !6.25 !dia_disc/2. + 1.25 ! part 1, circular part radius
    l_dv_x2 = 2.*r_dv_x1 - 0.77        ! the position, x value where part 2 start
    l_dv_x3 = l_dv_x2 + l_dv_neck     ! the position, x value where part 3 start 
    
    ! --------------- discharge reed valve parameter -------------
    ratio_val_port = dia_dv/dia_disc    ! ratio x = r_o/r_i, from oil stiction paper
    
    ! =========================================================
    ! the following part is for rectangular valve
    ! not using in NVC ver 1-1
    ! ---------------------------------------------------------
    read(110,*) w_dv_rec
    read(110,*) l_dv_rec
    l_dv_rec_ef = l_dv_rec - 8.0
    !w_dv_1 = 14.0  !6.0
    !t_dv_1 = 0.03
    !l_dv_1 = 25.0
    !x_hole_start_1 = (l_dv_1 - 5.0) - 2.0 - dia_disc1           ! Distance from screw to discharge port
    !r_dv1_x3 = dia_disc1/2.       ! Radius of third section of valve plate
         
    !x_hole_start_1 = l_dv1.df - 2.*r_dv1_x3       ! l_dv1x2
    !l_dv1.df_x1 = x_hole_start_1 - 10.
    !w_dv1_x2 = 4.0
    
    !y_stop_1 = 4.0           ! Maximum deflection
    ! ==========================================================
    
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Main Dimensions of Compressor  "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) "Shaft Radius                   r_shaft    = ", r_shaft, " mm"
    write (113,2989) "Outer Cylinder Radius          r_oc       = ", r_oc, " mm"
    write (113,2989) "Rotor Outer Radius             r_ro       = ", r_ro, " mm"
    write (113,2989) "Rotor Inner Radius             r_roi      = ", r_roi, " mm"
    write (113,2989) "Vane Head Radius               r_vh       = ", r_vh, " mm"
    write (113,2989) "Rotor Thickness                t_ro       = ", t_ro, " mm"
    write (113,2989) "Outer Cylinder Thickness       t_oc       = ", t_oc, " mm"
    write (113,2989) "Eccentricity                   e          = ", e, " mm"
    write (113,2989) "Eccentric Diameter             r_ecc      = ", r_ecc, " mm"
    write (113,2989) "Eccentric Length               l_ecc      = ", l_ecc, " mm"
    write (113,2989) "Length of compressor           l_com      = ", l_com, " mm"
    write (113,2989) "Length of Shoulder             l_vh       = ", l_vh, " mm"
    write (113,2989) "Length of vane                 l_v_ro     = ", l_v_ro, " mm"
    write (113,2989) "Length of vane (max)           l_v_max    = ", l_v_max," mm"
    write (113,2989) "Length of upper bearing        l_bearing  = ", l_bearing, " mm"
    write (113,2989) "Width of Shoulder              w_vs_ro    = ", w_vs_ro, " mm"
    write (113,2989) "Width of vane                  w_v_ro     = ", w_v_ro, " mm"
    write (113,2989) "Width of vane (max)            w_v_max    = ", w_v_max," mm"
    write (113,2989) "Width of vane slot gap         w_vsro_gap = ", w_vsro_gap, " mm"
    write (113,2989) "Length of vane slot gap        l_vsro_gap = ", l_vsro_gap, " mm"
    write (113,*) ' '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*)    " Port Dimensions "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) "Suction Port Diameter             d_suc             = ", dia_suc, " mm"
    write (113,2989) "Discharge Port Diameter           d_disc            = ", dia_disc, " mm"
    write (113,2989) "Suction Port Length               length_suc        = ", length_suc, " mm"
    write (113,2989) "Discharge Port Length             length_disc       = ", length_disc, " mm"
    write (113,2989) "Suction Port start angle          theta_suc_start   = ", theta_suc_start*180.0/pi, " degree"
    write (113,2989) "Discharge Port close angle        theta_disc_start  = ", theta_disc_start*180.0/pi, " degree"
    write (113,2989) "Suction Port fully open angle     theta_suc_end     = ", theta_suc_end*180.0/pi, " degree"
    write (113,2989) "Discharge Port fully close angle  theta_disc_end    = ", theta_disc_end*180.0/pi, " degree"
    write (113,*)
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Discharge Valve Dimensions "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) "Total Length of Discharge Valve        l_dv         = ", l_dv, " mm"
    write (113,2989) "Eff. Length of Discharge Valve         l_dv_ef      = ", l_dv_ef, " mm"
    write (113,2989) "Length of Discharge Valve Part x1                   = ", 2*r_dv_x1 - 0.77, " mm"
    write (113,2989) "Diameter of Discharge Valve Part x1    dia_dv       = ", dia_dv, " mm"
    write (113,2989) "Length of Discharge Valve Part x2                   = ", l_dv_neck, " mm"
    write (113,2989) "Width of Discharge Valve Part x2       w_dv_x2      = ", w_dv_x2, " mm"
    write (113,2989) "Length of Discharge Valve Part x3                   = ", l_dv_ef - l_dv_x3, " mm"
    write (113,2989) "Width of Discharge Valve Part x3       w_dv         = ", w_dv, " mm"
    write (113,2989) "Thickness of Discharge Valve           t_dv         = ", t_dv, " mm"
    
    !write (113,2989) "Distance from Fixed end to Port    x_hole_start_1  = ", x_hole_start_1, " mm"
    write (113,2989) "Maximum Deflection                     y_stop       = ", y_stop, " mm"
    

2989 format (2x,A,F8.4,A)
end subroutine main_dimension