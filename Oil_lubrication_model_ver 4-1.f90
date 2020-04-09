subroutine oil_lub_network_ver4_1(theta_1, theta_2, p_dcv, p_scv, eccr)
    
    ! ------------------------
    ! Oil lubrication network version 2-0
    ! Modified by Y.D. Lim on 
    ! Modified on 03-October-2018
    ! ------------------------
    
    implicit none
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"
    include "var_main_dimensions.f90"
    include "var_operating_fluid_condition.f90"
    include "var_clearance.f90"
   
    ! ------- routine parameter ----- !
    integer m, i, j, l
    ! ------- input --------- ! 
    double precision, dimension (1:no_data+1) :: theta_1, theta_2, p_dcv, p_scv, eccr
    !double precision R1, R2, R3, R4, del_P4, R5, Rsg5, R6, del_P6, R7, del_P7, R8c, R8s, R9, Rsg9, R10c, R10s, R11, del_P11, R12, Rsg12       ! resistances & potential differences
    double precision R2, R3, R4, R5, R6, R7c, R7s, R7, R7sg, R8c, R8s, R8, R9, R9sg    ! version 2   ! resistances & potential differences
    double precision del_P6, del_P4, del_P8     ! version 2
    !double precision L12, L34, L35, L45, L310    ! heights of path 1-2, 3-4, 3-5, 3-10 
    double precision h1, h3, h5, h36, h7, h7sg, h9, h9sg    ! version 2 
    double precision dia_feed_vert, cl_gap_ecc_up, cl_gap_ecc_low, num_R2         ! diameter of shaft feeding hole of path 3-4-5 ! eccentric clearance gap ! number of R1 
    double precision L_feed_radial_jour, dia_feed_radial_jour_45, dia_feed_radial_jour_56        ! for path 5-6, length and diameter at the journal radial feeding hole
    double precision L_feed_lb, dia_feed_lb         ! for path 2-3  ! length and diameter of the feeding hold of lower bearing component
    double precision depth_sg, width_sg, depth_sg_lower, width_sg_lower, depth_sg_ecc, width_sg_ecc, angle_sg, angle_sg_lower, pitch_sg, pitch_sg_lower, area_sg, area_sg_lower, area_sg_ecc, angle_sg_eccentric, pitch_sg_eccentric       ! for spiral grooves (paths 6-7, 8-9, 10-3)
    double precision p_reservoir    ! Housing pressure
    !double precision r_max_rotoredge      ! max distance from shaft center to edge of rotor (THE OUTER ONE)
    double precision r_max_rolleredge, r_min_rolleredge, r_average_rolleredge       ! max distance from shaft cetner to edge of roller (THE INNER ONE)
    ! ------ input for matrix ------ !
    integer, parameter :: n = 13 !n = 9 if it is 9 equations 9 unknowns (version 1-0)
    double precision mat_Q_element(n,n), mat_B(n), sol_Q(n)
    ! ------ Output ------- !
    double precision, dimension (1:no_data+1) :: Q1, Q2, Q3c, Q3s, Q4, Q5a, Q5b, Q6c, Q6s, Q7, Q8, Q9a, Q9b
    double precision, dimension (1:no_data+1) :: P2, P3, P4, P5, P6, P7, P8, P9, P3a
    ! ------------------------
    ! Read dimensions from file   -- 
    ! open(unit = 116, file = "Input Oil Lubrication Dimensions.txt", status = 'OLD')
    ! ------------------------
    read(116,*) h1		! [mm] Height of the path 1-2, or depth of the oil reservoir
    read(116,*) L_feed_lb	! [mm] Length of feeding hole at lower bearing
    read(116,*) dia_feed_lb	! [mm] Diameter of feeding hole at lower bearing
    read(116,*) dia_feed_vert	! [mm] Diameter of vertical feeding hole in the shaft 
    read(116,*)	h9		! [mm] Length of path 3-9, which is also the length of lower bearing (version 2)
    read(116,*)	h36		! [mm] Length of path 3-6, length of the shaft bottom (node 3) to bottom of upper bearing (node 6) (version 2)
    read(116,*)	h3		! [mm] Length of path 3-4, Length of the shaft end to the top feeding hole (version 2) !22.0,	! [mm]	L45		! Length of path 4-5, roughly same as length of the bearing, but is the distance between two feeding holes
    read(116,*)	L_feed_radial_jour	! [mm] Length of radial feeding hole at path 4-5 (version 2)
    read(116,*)	dia_feed_radial_jour_45	! [mm] Diameter of radial feeding hole at path 4-5 (version 2)
    read(116,*) dia_feed_radial_jour_56 ! [mm] Diameter of radial feeding hole at path 5-6
    read(116,*)	depth_sg	! [mm] Depth of the spiral groove
    read(116,*)	width_sg	! [mm] Width of the spiral groove
    read(116,*) depth_sg_lower ! [mm] Depth of the spiral groove at lower shaft
    read(116,*) width_sg_lower ! [mm] Width of the spiral groove at lower shaft
    read(116,*) depth_sg_ecc    ! [mm] depth eccentric sg
    read(116,*) width_sg_ecc    ! [mm] width eccentric sg
    read(116,*)	pitch_sg	! [mm] Pitch of the spiral groove
    read(116,*) pitch_sg_lower  ! [mm] Pitch of spiral groove at lower shaft
    read(116,*) pitch_sg_eccentric  ! [mm] Pitch of the spiral groove at eccentric
    read(116,*) p_reservoir     ! [kPa] Shell Pressure (Pressure at housing chamber)
    read(116,*) cl_gap_ecc_up  ! [mm] clearance gap of eccentric endface (upper)
    read(116,*) cl_gap_ecc_low  ! [mm] clearance gap of eccentric endface (lower)
    read(116,*) num_R2      ! Number of lower bearing feeding holes
    p_reservoir = p_disc
    close(116)
    
    ! Definition of some terms based on inputs
    ! Definition of each Flow Resistance (R) (only constant terms in this block)
    ! definition is found and completed by Z.X. Chan (March 2018)
    ! ---------------------------------------
    ! Variables that are not used in version 2 is kept in this box
    ! ----------Start here ------------
    ! angle_sg = atan(2.*pi*r_shaft/pitch_sg)     ! [rad] Calculate spiral groove angle (helix angle formula)
    ! area_sg = 0.5*pi*(0.5*depth_sg*1.d-3)**2   ! [m2] depth_sg*width_sg*1.d-6  x-sectional area of spiral groove (rectangular)
    ! R3 = (8.*miu_oil*L45*1.d-3)/(pi*((0.5*dia_feed_vert*1.d-3)**4))
    ! R6 = (8.*miu_oil*L_feed_radial_jour*1.d-3)/(pi*((0.5*dia_feed_radial_jour_47*1.d-3)**4))
    ! del_P6 = 0.5*rho_oil*(omega_1**2)*(((r_shaft*1.d-3)**2) - ((0.5*dia_feed_vert*1.d-3)**2))
    ! ---------------------------------------
    ! ------ Version 2 variables ------------
    
    angle_sg_lower = atan(2.*pi*r_shaft/pitch_sg_lower)     ! [rad] Calculate spiral groove angle (helix angle formula)
    angle_sg_eccentric = atan(2.*pi*r_roi/pitch_sg_eccentric)     ! [rad] spiral groove angle of the eccentric (roller)
    area_sg_lower = depth_sg_lower*width_sg_lower*1.d-6  !0.5*pi*(0.5*depth_sg_lower*1.d-3)**2 ! depth_sg_lower*width_sg_lower*1.d-6     ! [m2] x-sectional area for lower shaft spiral groove (rectangle)
    area_sg_ecc = depth_sg_ecc*width_sg_ecc*1.d-6   !0.5*pi*(0.5*depth_sg_ecc*1.d-3)**2 !depth_sg_ecc*width_sg_ecc*1.d-6   ! [m2] eccentric sg area
    
    h5 = h3 - h36   ! [mm] Length of path 5-6
    h7 = l_com  ! assign definition according to equation
    h7sg = h7!*cos(angle_sg_eccentric)
    !h9 = l_bearing
    h9sg = h9!*cos(angle_sg_lower)
    
    r_max_rolleredge = e + r_roi    ! [mm]
    r_min_rolleredge = r_shaft !r_roi - e    ! [mm]
    r_average_rolleredge = 0.5*(r_max_rolleredge + r_min_rolleredge)    ! [mm] Assumed average length of flow path
    
    R2 = (8.*miu_oil*L_feed_lb*1.d-3)/(pi*((0.5*dia_feed_lb*1.d-3)**4))     ! flow resistance of a single feeding hole
    R2 = R2/num_R2      ! Many feeding holes (treated as parallel resistance)
    R3 = (8.*miu_oil*h3*1.d-3)/(pi*((0.5*dia_feed_vert*1.d-3)**4))
    R4 = (8.*miu_oil*L_feed_radial_jour*1.d-3)/(pi*((0.5*dia_feed_radial_jour_45*1.d-3)**4))
    R4 = R4 / 2.    ! there are two feeding hole at upper bearing
    R6 = 6.*miu_oil*log(r_max_rolleredge/r_shaft)/(pi*(cl_gap_ecc_up*1.d-3)**3)  ! prototype version 2-3, large eccentric height
    R7sg = 12.*miu_oil*(l_ecc)*1.d-3/(((depth_sg_ecc*1.d-3)**3)*width_sg_ecc*1.d-3*cos(angle_sg_eccentric))   ! note: same eccentric groove width/depth and upper shaft groove width/depth 
    R8 =  6.*miu_oil*log(r_max_rolleredge/r_shaft)/(pi*(cl_gap_ecc_low*1.d-3)**3)  ! prototype version 2-3, large eccentric height
    R9sg = 12.*miu_oil*(l_bearing*1.d-3)/(((depth_sg_lower*1.d-3)**3)*width_sg_lower*1.d-3*cos(angle_sg_lower))
    
    del_P4 = 0.5*rho_oil*(omega_1**2)*(((r_shaft*1.d-3)**2) - ((0.5*dia_feed_vert*1.d-3)**2))     ! Pressure difference across path 4-5
    del_P6 = 0.5*rho_oil*(omega_1**2)*(((r_average_rolleredge*1.d-3)**2) - ((r_shaft*1.d-3)**2)) 
    del_P8 = 0.5*rho_oil*(omega_1**2)*(((r_average_rolleredge*1.d-3)**2) - ((r_shaft*1.d-3)**2)) 
    
    do i = 1, no_data+1
        ! -------------------------------
        ! Define Flow Resistance (Variables)
        ! -------------------------------
        R5 = 12.*miu_oil*l_bearing*1.d-3/((cl_bear_s_up**3)*pi*r_bearing*1.d-3*(1. + 1.5*eccr(i)**2))
        
        R7c = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_upend**3)*(2.*pi/(2.*pi - theta_2(i) - 0.00000001))
        R7s = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_upend**3)*(2.*pi/(theta_2(i) + 0.00000001))

        R7 = 12.*miu_oil*l_com*1.d-3/((cl_rad_roll**3)*pi*r_roi*1.d-3*(1. + 1.5*eccr(i)**2))
        
        R8c = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_lowend**3)*(2.*pi/(2.*pi - theta_2(i) - 0.00000001))
        R8s = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_lowend**3)*(2.*pi/(theta_2(i) + 0.00000001))

        R9 = 12.*miu_oil*l_bearing*1.d-3/((cl_bear_s**3)*pi*r_bearing*1.d-3*(1. + 1.5*eccr(i)**2))
        
        ! --------------------------------------------------------
        ! initialize the matrix by reset it to be all zeros
        ! --------------------------------------------------------
        do m = 1,n
            do j = 1,n
                mat_Q_element(m,j) = 0.0        ! create a zero matrix first
            enddo
            mat_B(m) = 0.0
        enddo
    
        ! ----------------------------------------
        ! Assign resistance matrix elements value and mat_B (right hand side matrix)
        ! i.e. A31 = mat_Q_element(3,1) ... A_ij = mat_Q_element(i,j)
        ! i.e. B03 = mat_B(3)
        ! ----------------------------------------
    
        !Equation 1
        !mat_Q_element(1,1) = -1.0
        !mat_Q_element(1,6) = 1.0
        !mat_Q_element(1,7) = 1.0
        !mat_Q_element(1,11) = 1.0
        !mat_Q_element(1,12) = 1.0
        ! Equation 1 (node 3)
        mat_Q_element(1,1) = 1.0
        mat_Q_element(1,2) = -1.0
        mat_Q_element(1,11) = 1.0
        mat_Q_element(1,12) = 1.0
        mat_Q_element(1,13) = -1.0
        
        !Equation 2 (node 7)
        mat_Q_element(2,2) = 1.0
        mat_Q_element(2,3) = -1.0
        mat_Q_element(2,4) = -1.0
        mat_Q_element(2,5) = -1.0
        mat_Q_element(2,6) = -1.0
        mat_Q_element(2,7) = 1.0
        
        !Equation 3 (node 8)
        mat_Q_element(3,5) = 1.0
        mat_Q_element(3,6) = 1.0
        mat_Q_element(3,7) = -1.0
        mat_Q_element(3,8) = -1.0
        mat_Q_element(3,9) = -1.0
        mat_Q_element(3,10) = 1.0
        
        !Equation 4 (node 9)
        mat_Q_element(4,10) = -1.0
        mat_Q_element(4,11) = -1.0
        mat_Q_element(4,12) = -1.0
        mat_Q_element(4,13) = 1.0
        
        !Equation 5 (KVL on working chamber @ upper rotor/piston endface)
        mat_Q_element(5,3) = -R7c
        mat_Q_element(5,4) = R7s
        
        !Equation 6 (KVL on working chamber @ lower rotor/piston endface)
        mat_Q_element(6,8) = -R8c
        mat_Q_element(6,9) = R8s
        
        !Equation 7 KVL on Spiral Groove, Eccentric-Piston bearing clearance  
        mat_Q_element(7,5) = R7
        mat_Q_element(7,6) = -R7sg
        
        !Equation 8 KVL on Spiral Groove, Lower journal bearing bearing clearance
        mat_Q_element(8,11) = R9
        mat_Q_element(8,12) = -R9sg
 
        !Equation 9 path 1-2-3-4-5-6-7-c (Surface to Compression Chamber via upper end face)    
        mat_Q_element(9,1) = R2
        mat_Q_element(9,2) = R3 + R4 + R5 + R6
        mat_Q_element(9,3) = R7c
    
        !Equation 10 path 1-2-3-4-5-6-7-8-c (Surface to Compression Chamber via lower end face)                
        mat_Q_element(10,1) = R2
        mat_Q_element(10,2) = R3 + R4 + R5 + R6
        mat_Q_element(10,5) = R7
        mat_Q_element(10,8) = R8c
        
        !Equation 11 path 3-4-5-6-7-8-9-3
        mat_Q_element(11,2) = R3 + R4 + R5 + R6
        mat_Q_element(11,5) = R7
        mat_Q_element(11,10) = -R8
        mat_Q_element(11,11) = R9

        !Equation 12 Spiral groove (eccentric)
        mat_Q_element(12,7) = 1.0
        
        !Equation 13 Spiral groove (lower bearing)
        mat_Q_element(13,13) = 1.0
        
        mat_B(5) = (p_dcv(i) - p_scv(i))*1.d3  !Pc - Ps
        mat_B(6) = (p_dcv(i) - p_scv(i))*1.d3
        mat_B(7) = rho_oil*g_grav*(h7 - h7sg)*1.d-3 ! spiral groove loop eccentric
        mat_B(8) = rho_oil*g_grav*(h9 - h9sg)*1.d-3 ! spiral groove loop lower bearing
        mat_B(9) = (p_reservoir - p_dcv(i))*1.d3 + rho_oil*g_grav*(h1 - h3 + h5)*1.d-3 + del_P4 + del_P6 !Equation 9 path 1-2-3-4-5-6-7-c (Surface to Compression Chamber via upper end face)  
        mat_B(10) = (p_reservoir - p_dcv(i))*1.d3 + rho_oil*g_grav*(h1 - h3 + h5 + h7)*1.d-3 + del_P4 + del_P6  !Equation 10 path 1-2-3-4-5-6-7-8-c (Surface to Compression Chamber via lower end face)   
        mat_B(11) = rho_oil*g_grav*(-h3 + h5 + h7 + h9)*1.d-3 + del_P4  + del_P6 - del_P8    !Equation 11 path 3-4-5-6-7-8-9-3
        mat_B(12) = 0.5*area_sg_ecc*r_roi*1.d-3*omega_1*sin(angle_sg_eccentric) !Equation 12 Spiral groove (eccentric) ! ------ ECCENTRIC ROTATION
        mat_B(13) = 0.5*area_sg_lower*r_shaft*1.d-3*omega_1*sin(angle_sg_lower) !Equation 13 Spiral groove (lower bearing)
        
        !write(99,2982) theta_1(i)*180.0/pi, mat_B
        ! -----------------------------------
        ! Code checking
        ! to see if the matrix is correct, uncomment below
        ! -----------------------------------
        !do l=1,n
        !    write (6,201) (mat_Q_element(l,j),j=1,n), mat_B(l)
        !end do
        !   write(6,*)' '
        !pause
        
        ! ----- Solve the matrix using Gauss Elimination -----
        call Gauss_elimination(mat_Q_element, mat_B, sol_Q, n)
        !do l=1,n
        !    write (6,201) (mat_Q_element(l,j),j=1,n), mat_B(l)
        !end do
        !   write(6,*)' '
        !pause

        ! ---------------------------
        ! Assign solution to each flow Q
        ! ---------------------------
        Q1(i) = sol_Q(1)
        Q2(i) = sol_Q(2)
        Q3c(i) = sol_Q(3)
        Q3s(i) = sol_Q(4)
        Q4(i) = sol_Q(5) 
        Q5a(i) = sol_Q(6) 
        Q5b(i) = sol_Q(7) 
        Q6c(i) = sol_Q(8) 
        Q6s(i) = sol_Q(9) 
        Q7(i) = sol_Q(10) 
        Q8(i) = sol_Q(11) 
        Q9a(i) = sol_Q(12) 
        Q9b(i) = sol_Q(13)

        ! --- Calculate pressures at each elements
        P2(i) = p_reservoir*1.d3 + rho_oil*g_grav*h1*1.d-3
        P3(i) = P2(i) - Q1(i)*R2
        P4(i) = P3(i) - rho_oil*g_grav*h3*1.d-3 - Q2(i)*R3
        P5(i) = P4(i) - Q2(i)*R4 + del_P4
        P6(i) = P5(i) - Q2(i)*R4 + rho_oil*g_grav*h5*1.d-3
        P7(i) = P4(i) - Q2(i)*R6 + del_P6
        P8(i) = P7(i) - Q4(i)*R7 + rho_oil*g_grav*h7*1.d-3
        P9(i) = P8(i) - Q7(i)*R8 - del_P8
        P3a(i) = P9(i) - Q8(i)*R9 +  rho_oil*g_grav*h9*1.d-3
        !P3b(i) = P10(i) - Q12(i)*R12 + rho_oil*g_grav*(l_bearing)*(1.d-3)

    enddo
    
    
    ! ----------------------------------
    ! Write header and data into file
    ! ----------------------------------
    write (141,2981), "Degree", "Q1[m3/s]", "Q2[m3/s]", "Q3c[m3/s]","Q3s[m3/s]", "Q4[m3/s]", "Q5a[m3/s]", "Q5b[m3/s]", "Q6c[m3/s]", "Q6s[m3/s]", "Q7[m3/s]", "Q8[m3/s]", "Q9a[m3/s]", "Q9b[m3/s]"
    write (142,2981), "Degree", "P1[Pa]", "P2[Pa]", "P3[Pa]", "P4[Pa]", "P5[Pa]", "P6[Pa]", "P7[Pa]", "P8[Pa]", "P9[Pa]", "P3a[Pa]"
    do i = 1, no_data+data_step, data_step
        
        write(141,2982) theta_1(i)*180.0/pi, Q1(i), Q2(i), Q3c(i), Q3s(i), Q4(i), Q5a(i), Q5b(i), Q6c(i), Q6s(i), Q7(i), Q8(i), Q9a(i), Q9b(i)
        write(142,2982) theta_1(i)*180.0/pi, p_reservoir*1000, P2(i), P3(i), P4(i), P5(i), P6(i), P7(i), P8(i), P9(i), P3a(i)
    enddo
    ! -----------------------------------------
    ! To check oil flow rate,
    ! refer to the ppt made "Oil network version 2-0.pptx"
    ! for lower bearing, total oil flow sohuld be Q9b-Q9a-Q8
    ! for eccentric, total oil flow rate Q5b - Q5a - Q4
    
201 format (20ES12.5)
2981 format (20A25)
2982 format (F25.4, 20ES25.6)

endsubroutine
    