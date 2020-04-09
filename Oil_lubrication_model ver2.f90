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
    double precision dia_feed_vert        ! diameter of shaft feeding hole of path 3-4-5
    double precision L_feed_radial_jour, dia_feed_radial_jour        ! for path 5-6, length and diameter at the journal radial feeding hole
    double precision L_feed_lb, dia_feed_lb         ! for path 2-3  ! length and diameter of the feeding hold of lower bearing component
    double precision depth_sg, width_sg, angle_sg, pitch_sg, area_sg       ! for spiral grooves (paths 6-7, 8-9, 10-3)
    double precision r_max_rotoredge      ! max distance from shaft center to edge of rotor (THE OUTER ONE)
    double precision r_max_rolleredge       ! max distance from shaft cetner to edge of roller (THE INNER ONE)
    ! ------ input for matrix ------ !
    integer, parameter :: n = 18 !n = 9 if it is 9 equations 9 unknowns (version 1-0)
    double precision mat_Q_element(n,n), mat_B(n), sol_Q(n)
    ! ------ Output ------- !
    double precision, dimension (1:no_data+1) :: Q1, Q2, Q3, Q4, Q5a, Q5b, Q6, Q7c, Q7s, Q8, Q9a, Q9b, Q10c, Q10s, Q11, Q12, Q13a, Q13b
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
    read(116,*)	dia_feed_radial_jour	! [mm] Diameter of radial feeding hole at path 4-7 and path 5-6
    read(116,*)	depth_sg	! [mm] Depth of the spiral groove
    read(116,*)	width_sg	! [mm] Width of the spiral groove
    read(116,*)	pitch_sg	! [mm] Pitch of the spiral groove

    ! ------------------------
    ! Definition of some terms based on inputs
    ! Definition of each Flow Resistance (R) (only constant terms in this block)
    ! definition is found and completed by Z.X. Chan (March 2018)
    ! ------------------------
    L45 = L35 - L34     ! [mm] Length of path 4-5
    angle_sg = atan(2.*pi*r_shaft/pitch_sg)     ! [rad] Calculate spiral groove angle (helix angle formula)
    area_sg = pi*(0.5*depth_sg*1.e-3)**2        ! [m2] x-sectional area of spiral groove
    r_max_rolleredge = e + r_roi    ! [mm]
    r_max_rotoredge = e + r_ro  ! [mm]
    
    R1 = pi*((0.5*dia_feed_lb*1.e-3)**4)/(8.*miu_oil*L_feed_lb*1.e-3) 
    R2 = pi*((0.5*dia_feed_vert*1.e-3)**4)/(8.*miu_oil*L35*1.e-3)
    R3 = pi*((0.5*dia_feed_vert*1.e-3)**4)/(8.*miu_oil*L45*1.e-3)
    R4 = pi*((0.5*dia_feed_radial_jour*1.e-3)**4)/(8.*miu_oil*L_feed_radial_jour*1.e-3)
    del_P4 = rho_oil*(omega_1**2)/2.*(((r_shaft*1.e-3)**2) - ((0.5*dia_feed_vert*1.e-3)**2))     ! Pressure difference across path 5-6
    
    Rsg5 = 12.*miu_oil*L45*1.e-3/((depth_sg*1.e-3)**3*width_sg*1.e-3*cos(angle_sg))
    Rsg9 = 12.*miu_oil*l_com*1.e-3/(((depth_sg*1.e-3)**3)*width_sg*1.e-3*cos(angle_sg))
    Rsg12 = 12.*miu_oil*(L310*1.e-3)/(((depth_sg*1.e-3)**3)*width_sg*1.e-3*cos(angle_sg))
    
    R6 = pi*((0.5*dia_feed_radial_jour*1.e-3)**4)/(8.*miu_oil*L_feed_radial_jour*1.e-3)
    del_P6 = 0.5*rho_oil*(omega_1**2)*(((r_shaft*1.e-3)**2) - ((0.5*dia_feed_vert*1.e-3)**2))
    
    R7 = 6.*miu_oil*log(r_max_rolleredge/r_shaft)/(pi*(cl_upend)**3)
    del_P7 = 0.5*rho_oil*(omega_1**2)*(((r_max_rolleredge*1.e-3)**2) - ((r_shaft*1.e-3)**2))   
    
    R11 =  6.*miu_oil*log(r_max_rolleredge/r_shaft)/(pi*cl_lowend**3)
    del_P11 = 0.5*rho_oil*(omega_1**2)*(((r_max_rolleredge*1.e-3)**2) - ((r_shaft*1.e-3)**2)) 
    
    do i = 1, no_data+1
        ! -------------------------------
        ! Define Flow Resistance (Variables)
        ! -------------------------------
        R5 = 12.*miu_oil*L45*1.e-3/((cl_bear_s)**3*pi*r_bearing*1.e-3*(1. + 1.5*eccr(i)**2))

        R8c = 6.*miu_oil*log(r_max_rotoredge/r_max_rolleredge)/(pi*cl_upend**3)*(2.*pi/(2.*pi - theta_2(i)))
        R8s = 6.*miu_oil*log(r_max_rotoredge/r_max_rolleredge)/(pi*cl_upend**3)*(2.*pi/(theta_2(i)+0.00000001))

        R9 = 12.*miu_oil*l_com*1.e-3/((cl_rad_roll**3)*pi*r_bearing*1.e-3*(1. + 1.5*eccr(i)**2))
        
        R10c = 6.*miu_oil*log(r_max_rotoredge/r_max_rolleredge)/(pi*cl_upend**3)*(2.*pi/(2.*pi - theta_2(i)))
        R10s = 6.*miu_oil*log(r_Max_rotoredge/r_max_rolleredge)/(pi*cl_upend**3)*(2.*pi/(theta_2(i)+0.00000001))

        R12 = 12.*miu_oil*l_com*1.e-3/((cl_bear_s**3)*pi*r_bearing*1.e-3*(1. + 1.5*eccr(i)**2))

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
    
        mat_Q_element(1,1) = 1.0
        mat_Q_element(1,8) = -1.0
        mat_Q_element(1,9) = -1.0
        mat_Q_element(1,13) = -1.0
        mat_Q_element(1,14) = -1.0
        
        ! EQ-2
        mat_Q_element(2,1) = 1.0
        mat_Q_element(2,4) = -1.0
        mat_Q_element(2,5) = -1.0
        mat_Q_element(2,6) = -1.0
        mat_Q_element(2,7) = -1.0
        mat_Q_element(2,16) = 1.0
        mat_Q_element(2,17) = 1.0
        
        !Omitted from Ver 1 and 3
        !mat_Q_element(2,1) = 1.0
        !mat_Q_element(2,2) = -1.0
        !mat_Q_element(2,16) = 1.0
        !mat_Q_element(2,17) = 1.0
        
        mat_Q_element(3,2) = 1.0
        mat_Q_element(3,8) = -1.0
        mat_Q_element(3,9) = -1.0
        mat_Q_element(3,10) = -1.0
        mat_Q_element(3,11) = -1.0
        mat_Q_element(3,12) = -1.0
        
        mat_Q_element(4,10) = 1.0
        mat_Q_element(4,11) = 1.0
        mat_Q_element(4,12) = 1.0
        mat_Q_element(4,13) = -1.0
        mat_Q_element(4,14) = -1.0
        mat_Q_element(4,15) = -1.0
        
        mat_Q_element(5,15) = 1.0
        mat_Q_element(5,16) = -1.0
        mat_Q_element(5,17) = -1.0
        mat_Q_element(5,18) = 1.0
        
        mat_Q_element(6,8) = -R8c
        mat_Q_element(6,9) = R8s
        
        mat_Q_element(7,13) = -R10c
        mat_Q_element(7,14) = R10s
        
        mat_Q_element(8,4) = R5
        mat_Q_element(8,5) = -Rsg5
        
        mat_Q_element(9,10) = R9
        mat_Q_element(9,11) = -Rsg9
        
        mat_Q_element(10,16) = R12
        mat_Q_element(10,17) = -Rsg12
        
        mat_Q_element(11,2) = 0.5*R1 + R2 + R7
        mat_Q_element(11,7) = 0.5*R6
        mat_Q_element(11,8) = R8c
        
        !Omitted from Ver2
        !mat_Q_element(12,2) = 0.5*R1 + R2 + R7
        !mat_Q_element(12,3) = 0.5*R4 + R3
        !mat_Q_element(12,4) = R5
        !mat_Q_element(12,8) = R8c
        !mat_Q_element(12,9) = R8s

        !Omitted from Ver 3
        !mat_Q_element(12,2) = 1.0
        !mat_Q_element(12,3) = -1.0
        !mat_Q_element(12,7) = -1.0
        
        mat_Q_element(12,2) = R2 + R7
        mat_Q_element(12,3) = R3 + R4/2
        mat_Q_element(12,4) = R5
        mat_Q_element(12,10) = R9
        mat_Q_element(12,15) = R11
        mat_Q_element(12,16) = R12
        
        ! Path 1-2-3-4-7-8-9-c
        mat_Q_element(13,2) = 0.5*R1 + R2 + R7
        mat_Q_element(13,7) = 0.5*R6
        mat_Q_element(13,10) = R9
        mat_Q_element(13,13) = R10c
        
        ! EQ 2c
        !mat_Q_element(13,3) = 1.0
        !mat_Q_element(13,4) = -1.0
        !mat_Q_element(13,5) = -1.0
        !mat_Q_element(13,6) = -1.0

        !Omitted from Ver1 and 3
        !mat_Q_element(14,2) = 0.5*R1 + R2 + R7
        !mat_Q_element(14,3) = 0.5*R4 + R3
        !mat_Q_element(14,4) = R5
        !mat_Q_element(14,10) = R9
        !mat_Q_element(14,13) = R10c
        
        
        ! path 4-5-6-7-4
        mat_Q_element(14,3) = R3 + R4
        mat_Q_element(14,4) = R5
        mat_Q_element(14,7) = -R6/2
        
        
        mat_Q_element(15,2) = R2 + R7
        mat_Q_element(15,7) = 0.5*R6
        mat_Q_element(15,10) = R9
        mat_Q_element(15,15) = R11
        mat_Q_element(15,16) = R12

        mat_Q_element(16,6) = 1.0
        mat_Q_element(17,12) = 1.0
        mat_Q_element(18,18) = 1.0
        
        mat_B(6) = (p_dcv(i) - p_scv(i))*1000.  !Pc - Ps
        mat_B(11) = rho_oil*g_grav*(L12-L34)*1.e-3 + del_P6 + del_P7 - p_dcv(i)*1000. + p_disc*1000.
        !Omitted from Ver2 and 3
        !mat_B(12) = rho_oil*g_grav*(L12-L34)*1.e-3 + del_P4 + del_P7 - p_scv(i)*1000. + p_disc*1000.
        mat_B(12) = rho_oil*g_grav*(-L34 + l_com + l_bearing)*1.e-3 + del_P4 + del_P7 - del_P11
        mat_B(13) = rho_oil*g_grav*(L12-L34 + l_com)*1.e-3 + del_P6 + del_P7 - p_dcv(i)*1000. + p_disc*1000.
        !Omitted from Ver1 and 3
        !mat_B(14) = rho_oil*g_grav*(L12-L34 + l_com)*1.e-3 + del_P7 - p_scv(i)*1000. + p_disc*1000.
        mat_B(14) = del_P4 - del_P6
        mat_B(15) = rho_oil*g_grav*(-L34 + l_com + l_bearing)*1.e-3 + del_P6 + del_P7 - del_P11
        mat_B(16) = 0.5*area_sg*r_shaft*1.e-3*omega_1*sin(angle_sg)
        mat_B(17) = 0.5*area_sg*r_roi*1.e-3*omega_1*sin(angle_sg) ! ------ ECCENTRIC ROTATION
        mat_B(18) = 0.5*area_sg*r_shaft*1.e-3*omega_1*sin(angle_sg)
        
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
        
    enddo
    
    
    ! ----------------------------------
    ! Write header and data into file
    ! ----------------------------------
    write (141,2981), "Degree", "Q1[m3/s]", "Q2[m3/s]", "Q3[m3/s]","Q4[m3/s]", "Q5a[m3/s]", "Q5b[m3/s]", "Q6[m3/s]", "Q7c[m3/s]", "Q7s[m3/s]", "Q8[m3/s]", "Q9a[m3/s]", "Q9b[m3/s]", "Q10c[m3/s]", "Q10s[m3/s]", "Q11[m3/s]", "Q12[m3/s]", "Q13a[m3/s]", "Q13b[m3/s]"
    do i = 1, no_data+data_step_suct, data_step_suct
        
        write(141,2982) theta_1(i)*180.0/pi, Q1(i), Q2(i), Q3(i), Q4(i), Q5a(i), Q5b(i), Q6(i), Q7c(i), Q7s(i), Q8(i), Q9a(i), Q9b(i), Q10c(i), Q10s(i), Q11(i), Q12(i), Q13a(i), Q13b(i)
    enddo
    
201 format (20ES12.5)
2981 format (20A25)
2982 format (F25.4, 20ES25.6E2)

endsubroutine