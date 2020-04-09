subroutine oil_lub_network(theta_1, theta_2, p_dcv, p_scv, eccr)
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
    double precision R1, R2, R3, R4, del_P4, R5, Rsg5, R6, del_P6, R7, del_P7, R8c, R8s, R9, Rsg9, R10c, R10s, R11, del_P11, R12, Rsg12       ! resistances & potential differences
    double precision L12, L34, L35, L45, L310    ! heights of path 1-2, 3-4, 3-5, 3-10 
    double precision dia_feed_vert, cl_gap_ecc, num_R1        ! diameter of shaft feeding hole of path 3-4-5 ! eccentric clearance gap
    double precision L_feed_radial_jour, dia_feed_radial_jour_47, dia_feed_radial_jour_56        ! for path 5-6, length and diameter at the journal radial feeding hole
    double precision L_feed_lb, dia_feed_lb         ! for path 2-3  ! length and diameter of the feeding hold of lower bearing component
    double precision depth_sg, width_sg, depth_sg_lower, width_sg_lower, depth_sg_ecc, width_sg_ecc, angle_sg, angle_sg_lower, pitch_sg, pitch_sg_lower, area_sg, area_sg_lower, area_sg_ecc, angle_sg_eccentric, pitch_sg_eccentric       ! for spiral grooves (paths 6-7, 8-9, 10-3)
    double precision p_reservoir    ! Housing pressure
    !double precision r_max_rotoredge      ! max distance from shaft center to edge of rotor (THE OUTER ONE)
    double precision r_max_rolleredge, r_min_rolleredge, r_average_rolleredge       ! max distance from shaft cetner to edge of roller (THE INNER ONE)
    ! ------ input for matrix ------ !
    integer, parameter :: n = 18 !n = 9 if it is 9 equations 9 unknowns (version 1-0)
    double precision mat_Q_element(n,n), mat_B(n), sol_Q(n)
    ! ------ Output ------- !
    double precision, dimension (1:no_data+1) :: Q1, Q2, Q3, Q4, Q5a, Q5b, Q6, Q7c, Q7s, Q8, Q9a, Q9b, Q10c, Q10s, Q11, Q12, Q13a, Q13b
    double precision, dimension (1:no_data+1) :: P2, P3, P4, P5, P6, P7, P8, P9, P10, P11
    ! ------------------------
    ! Read dimensions from file   -- 
    ! open(unit = 116, file = "Input Oil Lubrication Dimensions.txt", status = 'OLD')
    ! ------------------------
    read(116,*) L12		! [mm] Height of the path 1-2, or depth of the oil reservoir
    read(116,*) L_feed_lb	! [mm] Length of feeding hole at lower bearing
    read(116,*) dia_feed_lb	! [mm] Diameter of feeding hole at lower bearing
    read(116,*) dia_feed_vert	! [mm] Diameter of vertical feeding hole in the shaft 
    read(116,*)	L310		! [mm] Length of path 3-10, which is the length of lower bearing
    read(116,*)	L34		! [mm] Length of path 3-4, length of the shaft end to the middle feeding hole
    read(116,*)	L35		! [mm] Length of path 3-5, Length of the shaft end to the top feeding hole	!22.0,	! [mm]	L45		! Length of path 4-5, roughly same as length of the bearing, but is the distance between two feeding holes
    read(116,*)	L_feed_radial_jour	! [mm] Length of radial feeding hole at path 4-7 and path 5-6
    read(116,*)	dia_feed_radial_jour_47	! [mm] Diameter of radial feeding hole at path 4-7
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
    !read(116,*) p_reservoir     ! [kPa] Shell Pressure (Pressure at housing chamber)
    read(116,*) cl_gap_ecc  ! [mm] Pitch of the spiral groove at eccentric
    read(116,*) num_R1
    p_reservoir = p_disc
    close(116)
    ! ------------------------
    ! Definition of some terms based on inputs
    ! Definition of each Flow Resistance (R) (only constant terms in this block)
    ! definition is found and completed by Z.X. Chan (March 2018)
    ! ------------------------
    L45 = L35 - L34     ! [mm] Length of path 4-5
    angle_sg = atan(2.*pi*r_shaft/pitch_sg)     ! [rad] Calculate spiral groove angle (helix angle formula)
    angle_sg_lower = atan(2.*pi*r_shaft/pitch_sg_lower)     ! [rad] Calculate spiral groove angle (helix angle formula)
    angle_sg_eccentric = atan(2.*pi*r_roi/pitch_sg_eccentric)     ! [rad] spiral groove angle of the eccentric (roller)
    area_sg = depth_sg*width_sg*1.d-6  ! 0.5*pi*(0.5*depth_sg*1.d-3)**2   ! [m2] x-sectional area of spiral groove (rectangular)
    area_sg_lower = depth_sg_lower*width_sg_lower*1.d-6     ! [m2] x-sectional area for lower shaft spiral groove (rectangle)
    area_sg_ecc = depth_sg_ecc*width_sg_ecc*1.d-6   ! [m2] eccentric sg area
    r_max_rolleredge = e + r_roi    ! [mm]
    r_min_rolleredge = r_roi - e    ! [mm]
    r_average_rolleredge = 0.5*(r_max_rolleredge + r_min_rolleredge)    ! [mm] Assumed average length of flow path
    
    R1 = (8.*miu_oil*L_feed_lb*1.d-3)/(pi*((0.5*dia_feed_lb*1.d-3)**4))
    R2 = (8.*miu_oil*L35*1.d-3)/(pi*((0.5*dia_feed_vert*1.d-3)**4))
    R3 = (8.*miu_oil*L45*1.d-3)/(pi*((0.5*dia_feed_vert*1.d-3)**4))
    R4 = (8.*miu_oil*L_feed_radial_jour*1.d-3)/(pi*((0.5*dia_feed_radial_jour_56*1.d-3)**4))
    del_P4 = 0.5*rho_oil*(omega_1**2)*(((r_shaft*1.d-3)**2) - ((0.5*dia_feed_vert*1.d-3)**2))     ! Pressure difference across path 5-6
    
    Rsg5 = 12.*miu_oil*L45*1.d-3/((depth_sg*1.d-3)**3*width_sg*1.d-3*cos(angle_sg))
    Rsg9 = 12.*miu_oil*l_com*1.d-3/(((depth_sg_ecc*1.d-3)**3)*width_sg_ecc*1.d-3*cos(angle_sg_eccentric))   ! note: same eccentric groove width/depth and upper shaft groove width/depth 
    Rsg12 = 12.*miu_oil*(L310*1.d-3)/(((depth_sg_lower*1.d-3)**3)*width_sg_lower*1.d-3*cos(angle_sg_lower))
    
    R6 = (8.*miu_oil*L_feed_radial_jour*1.d-3)/(pi*((0.5*dia_feed_radial_jour_47*1.d-3)**4))
    del_P6 = 0.5*rho_oil*(omega_1**2)*(((r_shaft*1.d-3)**2) - ((0.5*dia_feed_vert*1.d-3)**2))
    
    R7 = 6.*miu_oil*log(r_average_rolleredge/r_shaft)/(pi*(cl_gap_ecc)**3)  ! prototype version 2-3, smaller eccentric height
    !R7 = 6.*miu_oil*log(r_average_rolleredge/r_shaft)/(pi*(cl_upend)**3)
    del_P7 = 0.5*rho_oil*(omega_1**2)*(((r_average_rolleredge*1.d-3)**2) - ((r_shaft*1.d-3)**2))   
    
    R11 =  6.*miu_oil*log(r_average_rolleredge/r_shaft)/(pi*cl_gap_ecc**3)  ! prototype version 2-3, smaller eccentric height
    !R11 =  6.*miu_oil*log(r_average_rolleredge/r_shaft)/(pi*cl_lowend**3)
    del_P11 = 0.5*rho_oil*(omega_1**2)*(((r_average_rolleredge*1.d-3)**2) - ((r_shaft*1.d-3)**2)) 
    
    do i = 1, no_data+1
        ! -------------------------------
        ! Define Flow Resistance (Variables)
        ! -------------------------------
        R5 = 12.*miu_oil*L45*1.d-3/((cl_bear_s_up**3)*pi*r_bearing*1.d-3*(1. + 1.5*eccr(i)**2))

        R8c = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_upend**3)*(2.*pi/(2.*pi - theta_2(i) - 0.00000001))
        R8s = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_upend**3)*(2.*pi/(theta_2(i)+0.00000001))

        R9 = 12.*miu_oil*l_com*1.d-3/((cl_rad_roll**3)*pi*r_roi*1.d-3*(1. + 1.5*eccr(i)**2))
        
        R10c = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_upend**3)*(2.*pi/(2.*pi - theta_2(i) - 0.00000001))
        R10s = 6.*miu_oil*log(r_ro/r_average_rolleredge)/(pi*cl_upend**3)*(2.*pi/(theta_2(i)+0.00000001))

        R12 = 12.*miu_oil*L45*1.d-3/((cl_bear_s**3)*pi*r_bearing*1.d-3*(1. + 1.5*eccr(i)**2))

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
        mat_Q_element(1,1) = -1.0
        mat_Q_element(1,8) = 1.0
        mat_Q_element(1,9) = 1.0
        mat_Q_element(1,13) = 1.0
        mat_Q_element(1,14) = 1.0
        
        !Equation 2
        mat_Q_element(2,1) = 1.0
        mat_Q_element(2,2) = -1.0
        mat_Q_element(2,16) = 1.0
        mat_Q_element(2,17) = 1.0
        mat_Q_element(2,18) = -1.0

        
        !Equation 3
        mat_Q_element(3,2) = 1.0
        mat_Q_element(3,3) = -1.0
        mat_Q_element(3,7) = -1.0
        
        !Equation 4
        mat_Q_element(4,2) = -1.0
        mat_Q_element(4,4) = 1.0
        mat_Q_element(4,5) = 1.0
        mat_Q_element(4,6) = 1.0
        mat_Q_element(4,7) = 1.0
        
        !Equation 5
        mat_Q_element(5,2) = 1.0
        mat_Q_element(5,8) = -1.0
        mat_Q_element(5,9) = -1.0
        mat_Q_element(5,10) = -1.0
        mat_Q_element(5,11) = -1.0
        mat_Q_element(5,12) = -1.0
        
        !Equation 6
        mat_Q_element(6,15) = 1.0
        mat_Q_element(6,16) = -1.0
        mat_Q_element(6,17) = -1.0
        mat_Q_element(6,18) = 1.0
        
        !Equation 7 KVL on Working Chamber, Upper End Face clearance
        mat_Q_element(7,8) = -R8c
        mat_Q_element(7,9) = R8s
        
        !Equation 8 KVL on Working Chamber, Upper End Face clearance
        mat_Q_element(8,13) = -R10c
        mat_Q_element(8,14) = R10s
        
        !Equation 9 KVL on Spiral Groove, journal bearing clearance
        mat_Q_element(9,4) = -R5
        mat_Q_element(9,5) = Rsg5
        
        !Equation 10 KVL on Spiral Groove, Eccentric-Piston bearing clearance       
        mat_Q_element(10,10) = -R9
        mat_Q_element(10,11) = Rsg9
    
        !Equation 11 KVL on Spiral Groove, Eccentric-Piston bearing clearance               
        mat_Q_element(11,16) = -R12
        mat_Q_element(11,17) = Rsg12
        
        !Equation 12 path 1-2-3-4-7-8-c Surface to Compression Chamber via upper end face
        mat_Q_element(12,1) = R1/num_R1
        mat_Q_element(12,2) = R2 + R7
        mat_Q_element(12,7) = 0.5*R6
        mat_Q_element(12,8) = R8c

        !Equation 13 path 1-2-3-4-7-8-9-c Surface to Compression Chamber via lower end face
        mat_Q_element(13,1) = R1/num_R1
        mat_Q_element(13,2) = R2 + R7
        mat_Q_element(13,7) = 0.5*R6
        mat_Q_element(13,10) = R9
        mat_Q_element(13,13) = R10c
        
        !Equation 14 path 3-4-7-8-9-10-3
        mat_Q_element(14,2) = R2 + R7
        mat_Q_element(14,7) = 0.5*R6
        mat_Q_element(14,10) = R9
        mat_Q_element(14,15) = R11
        mat_Q_element(14,16) = R12

        !Equation 15 Path 4-5-6-7-4
        mat_Q_element(15,3) = R3 + 0.5*R4
        mat_Q_element(15,4) = R5
        mat_Q_element(15,7) = -R6/2.
        ! Equation 15 if Q6 is taken out
        !mat_Q_element(15,7) = 1.0
        
        mat_Q_element(16,6) = 1.0
        mat_Q_element(17,12) = 1.0
        mat_Q_element(18,18) = 1.0
        
        
        mat_B(7) = (p_dcv(i) - p_scv(i))*1000.  !Pc - Ps
        mat_B(8) = (p_dcv(i) - p_scv(i))*1000.
        mat_B(9) = rho_oil*g_grav*(L35-L34)*(1.d-3)*(cos(angle_sg) - 1.0)
        mat_B(10) = rho_oil*g_grav*(l_com)*(1.d-3)*(cos(angle_sg_eccentric) - 1.0)
        mat_B(11) = rho_oil*g_grav*(L310)*(1.d-3)*(cos(angle_sg_lower) - 1.0)
        mat_B(12) = rho_oil*g_grav*(L12-L34)*1.d-3 + del_P4 + del_P7 - p_dcv(i)*1000. + p_reservoir*1000.
        mat_B(13) = rho_oil*g_grav*(L12-L34 + l_com)*1.d-3 + del_P7 - p_dcv(i)*1000. + p_reservoir*1000. + del_P4 !Path 1-2-3-4-7-8-9-10-c
        mat_B(14) = rho_oil*g_grav*(-L34 + l_com + l_bearing)*1.d-3 + del_P7 - del_P11 + del_P4    ! Path 3-4-7-8-9-10-3
        mat_B(15) = del_P4 - del_P6           ! Path 4-5-6-7-4
        !mat_B(15) = 0.0     ! Q6 = 0
        mat_B(16) = 0.5*area_sg*r_shaft*1.d-3*omega_1*sin(angle_sg)
        mat_B(17) = 0.5*area_sg*r_roi*1.d-3*omega_1*sin(angle_sg_eccentric) ! ------ ECCENTRIC ROTATION
        mat_B(18) = 0.5*area_sg_ecc*r_shaft*1.d-3*omega_1*sin(angle_sg_lower)
        
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
        Q3(i) = sol_Q(3)
        Q4(i) = sol_Q(4)
        Q5a(i) = sol_Q(5)
        Q5b(i) = sol_Q(6) 
        Q6(i) = sol_Q(7) 
        Q7c(i) = sol_Q(8) 
        Q7s(i) = sol_Q(9) 
        Q8(i) = sol_Q(10) 
        Q9a(i) = sol_Q(11) 
        Q9b(i) = sol_Q(12) 
        Q10c(i) = sol_Q(13) 
        Q10s(i) = sol_Q(14) 
        Q11(i) = sol_Q(15)
        Q12(i) = sol_Q(16) 
        Q13a(i) = sol_Q(17) 
        Q13b(i) = sol_Q(18)
        
        ! --- Calculate pressures at each elements
        P2(i) = p_disc*1000.0 - rho_oil*g_grav*L12*1.d-3
        P3(i) = P2(i) - Q1(i)*0.5*R1
        P4(i) = P3(i) - rho_oil*g_grav*L34*1.d-3 - Q2(i)*R2
        P5(i) = P4(i) - Q3(i)*R3 - rho_oil*g_grav*L45*1.d-3
        P6(i) = P5(i) - Q3(i)*0.5*R4 + del_P4
        P7(i) = P4(i) - Q6(i)*0.5*R6 + del_P6
        P8(i) = P7(i) - Q2(i)*R7 + del_P7
        P9(i) = P8(i) - Q8(i)*R9 + rho_oil*g_grav*(l_com)*(1.d-3)
        P10(i) = P9(i) - Q11(i)*R11 - del_P11
        !P3b(i) = P10(i) - Q12(i)*R12 + rho_oil*g_grav*(l_bearing)*(1.d-3)
        
    enddo
    
    
    ! ----------------------------------
    ! Write header and data into file
    ! ----------------------------------
    write (141,2981), "Degree", "Q1[m3/s]", "Q2[m3/s]", "Q3[m3/s]","Q4[m3/s]", "Q5a[m3/s]", "Q5b[m3/s]", "Q6[m3/s]", "Q7c[m3/s]", "Q7s[m3/s]", "Q8[m3/s]", "Q9a[m3/s]", "Q9b[m3/s]", "Q10c[m3/s]", "Q10s[m3/s]", "Q11[m3/s]", "Q12[m3/s]", "Q13a[m3/s]", "Q13b[m3/s]"
    write (142,2981), "Degree", "P1[Pa]", "P2[Pa]", "P3[Pa]", "P4[Pa]", "P5[Pa]", "P6[Pa]", "P7[Pa]", "P8[Pa]", "P9[Pa]", "P10[Pa]"
    do i = 1, no_data+data_step, data_step
        
        write(141,2982) theta_1(i)*180.0/pi, Q1(i), Q2(i), Q3(i), Q4(i), Q5a(i), Q5b(i), Q6(i), Q7c(i), Q7s(i), Q8(i), Q9a(i), Q9b(i), Q10c(i), Q10s(i), Q11(i), Q12(i), Q13a(i), Q13b(i)
        write(142,2982) theta_1(i)*180.0/pi, p_disc*1000, P2(i), P3(i), P4(i), P5(i), P6(i), P7(i), P8(i), P9(i), P10(i)
    enddo
    
201 format (20ES12.5)
2981 format (20A25)
2982 format (F25.4, 20ES25.6)

endsubroutine