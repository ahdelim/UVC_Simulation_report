subroutine dynamic_m2(theta_1, theta_2, gamma_1, l_v, p_scv, p_dcv, dgammadtt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, T_inertia_ro, T_inertia_vh, T_com_C)
    implicit none
    include 'var_simulation_parameter.f90'
    include "var_operational_parameter.f90"
    include 'var_physical_constants.f90'
    include 'var_main_dimensions.f90'
    include 'var_kinematics.f90'
    include "var_geometrical.f90"
    include "var_thermo.f90"
    include 'var_compressor_evaluation.f90'
    include "var_dynamic.f90"       ! main variables for dynamic model is here
    
    double precision cl_bear_s, cl_rad_roll
    common/clearance_bear/cl_bear_s, cl_rad_roll
    integer i,j,k,m,l, tilt_case
    integer, parameter :: n = 6 !n = 9 if it is 9 equations 9 unknowns (version 1-0)
    double precision mat_force_element(n,n), mat_B(n), sol_X(n)
    double precision, dimension (1:no_data+1) :: F_vvh, F_AB, F_v, F_DA       ! Known/defined forces
    double precision F_vsro_1, F_vsro_2, F_slot_vh
    double precision l_AB, l_AH, F_AH(no_data+1)    ! dummy variables and average velocity (assumption)
    double precision I_sect_I, I_sect_II, I_sect_III, I_ro_I, I_ro_II, I_ro_III     ! dummy variables for moment of inertia calculation 
    double precision mass_sect_I, mass_sect_II, mass_sect_III, mass_ro, mass_v_ro, mass_vs_ro        ! dummy mass variables for inertia calculation
    !double precision, dimension (1:no_data+1) :: alpha_vh, alpha_ro
    ! --------------------------
    ! for version 1.0 with 9 unknowns (failed on 21-11-2017)
    ! version 2.0 is with 6 unknowns
    ! --------------------------
    !write (90,2981), "Degree","F_ox[N]","F_oy[N]","T_vhro[Nm]","M_o[Nm]","F_1_n[N]","F_2_n[N]","F_cx[N]","F_cy[N]","T_roll[Nm]"        ! version 1.0
    ! --------------------
    ! To counter the force-torque relationship
    ! If the normal force (F_1,n or F_2,n is positive), case 1, as the first analysis
    ! If the normal force (F_1,n or F_2,n is negative), go to case 2
    ! --------------------
    tilt_case = 1               ! Default as case 1
    ! --------------------
    ! Inertia of vane housing and mass of rotor
    ! --------------------
    mass_sect_I = rho_mat*pi*(r_vh*1.e-3)**2*l_com*1.e-3    ! [kg] mass of section I as stated in diagram
    mass_sect_II = rho_mat*(l_vh*1.e-3)*(2.*w_vs_ro + w_v_ro)*1.e-3*l_com*1.e-3 ! [kg] mass of section II
    mass_sect_III = rho_mat*(l_vs_ro + l_v_ro)*1.e-3*w_v_ro*1.e-3*l_com*1.e-3   ! [kg] mass of section III
    mass_ro = rho_mat*pi*((r_ro*1.e-3)**2 - (r_roi*1.e-3)**2)*l_com*1.e-3   ! [kg] mass of the rotor without the vane and vane slots
    mass_v_ro = rho_mat*l_v_ro*w_v_ro*l_com*1.e-9       ! [kg] mass of the vane protruded from the rotor from the rotor radius
    mass_vs_ro = rho_mat*l_vs_ro*w_vs_ro*l_com*1.e-9    ! [kg] assumed mass of the vane slot at the rotor
    
    I_sect_I = 0.5*mass_sect_I*(r_vh*1.e-3)**2
    I_sect_II = mass_sect_II*(1./12*((2.*w_vs_ro*1.e-3 + w_v_ro*1.e-3)**2 + (l_vh*1.e-3)**2) + (r_vh*1.e-3 + 0.5*l_vh*1.e-3)**2)
    I_sect_III = mass_sect_III*(1./12*(w_v_ro**2 + (l_vs_ro + l_v_ro)**2) + (0.5*(l_vs_ro + l_v_ro) - r_vh)**2)*1.e-6
    I_vh_O = I_sect_I + I_sect_II - I_sect_III      ! [kgm2] moment of inertia of vane housing
    
    ! store down mass of vane housing and rotor
    mass_vh = mass_sect_I + mass_sect_II - mass_sect_III        ! [kg] total mass of vane housing
    mass_rotor = mass_ro + mass_v_ro - mass_vs_ro               ! [kg] total mass of rotor
    
    ! --------------------------------------
    ! loop for solving the force matrix
    ! --------------------------------------
    do 123 i = 1,no_data+1
        ! --------------------------------------------------------
        ! initialize the matrix by reset it to be all zeros
        ! --------------------------------------------------------
        do m = 1,n
            do j = 1,n
                mat_force_element(m,j) = 0.0        ! create a zero matrix first
            enddo
        enddo
        ! ----------------------------------------
        ! Moment of inertia of rotor (varying due to different distance to point O)
        ! ----------------------------------------
        I_ro_I = (0.5*mass_ro*(r_ro**2 - r_roi**2) + mass_ro*(r_vh + l_v(i) + r_ro)**2)*1.e-6
        I_ro_II = (mass_v_ro/12.*(w_v_ro**2 + l_v_ro**2) + mass_v_ro*(r_vh + l_v(i) + 0.5*l_v_ro)**2)*1.e-6
        I_ro_III = 2.0*(mass_vs_ro/12.*(w_vs_ro**2 + l_vs_ro**2) + mass_vs_ro*((r_vh + l_v(i) + 0.5*l_vs_ro)**2 + (0.5*w_vs_ro + 0.5*w_v_ro)**2))*1.e-6
        I_ro_O(i) = I_ro_I + I_ro_II - I_ro_III     ! [kgm2] moment of inertia of rotor
        ! ----------------------------------------
        ! 1.    Geometrical known terms
        ! 2.    Known forces terms, refer to dynamic model FBD
        ! ----------------------------------------
        !l_AH = sqrt((r_ro + 0.5*(l_v(i) - l_vh))**2 + r_ro**2 - 2*(r_ro + 0.5*(l_v(i) - l_vh))*r_ro*cos(theta_2(i)))*1.e-3
        l_AB = 2*r_ro*1.e-3*sin(0.5*theta_2(i))     ! [m] projected length (area) where the pressure force acting on (2*r_ro*sin(0.5*theta2)
        !l_AB = sqrt((r_ro*1.e-3)**2 + ((r_ro + l_v(i) + r_vh)*1.e-3)**2 - 2*(r_ro*1.e-3)*((r_ro + l_v(i) + r_vh)*1.e-3)*cos(theta_2(i)))
        !F_AH(i) = 1000.*(p_dcv(i) - p_scv(i))*l_AH*(l_com*1.e-3)  
        F_AB(i) = 1000.*(p_dcv(i) - p_scv(i))*l_AB*(l_com*1.e-3)      ! [N]   projected pressure force acting on rotor body
        F_vsro_1 = p_scv(i)*l_com*w_vs_ro*1.e-3     ! [N] Force acting on the two side vanes
        F_vsro_2 = p_dcv(i)*l_com*w_vs_ro*1.e-3     ! [N] Force acting on the two side vanes
        F_slot_vh = 0.5*1000.0*(p_dcv(i) + p_scv(i))*l_com*w_v_ro*1.e-6   ! [N] Force acting on the middle vane housing rotor vane slot
        if (l_v(i) > l_vh) then
            F_vvh(i) = 1000.*(p_dcv(i) - p_scv(i))*(l_vh + l_exposed_vh)*l_com*1.e-6     ! [N] pressure force acting on vane of vane housing (from the radius of vane housing)
            F_v(i) = 1000.*(p_dcv(i) - p_scv(i))*(l_v(i) - l_vh)*l_com*1.e-6     ! [N]   projected pressure force acting on vane
        else
            F_vvh(i) = 1000.*(p_dcv(i) - p_scv(i))*(l_v(i) + l_exposed_vh)*l_com*1.e-6       ! case when the vane housing vane is in the slot
            F_v(i) = 0.0
        endif
        ! ----------------------------------------
        ! Assign force matrix elements value and mat_B
        ! i.e. A31 = mat_force_element(3,1) ... A_ij = mat_force_element(i,j)
        ! i.e. B03 = mat_B(3)
        ! ----------------------------------------
        !if (tilt_case == 1) then
!167         continue            
            mat_force_element(1,1) = cos(gamma_1(i))        ! + coef_vhs*sin(gamma_1(i))       ! A11
            mat_force_element(1,2) = - cos(gamma_1(i)) - coef_vs*sin(-gamma_1(i))        !   A12
            mat_force_element(1,3) = cos(gamma_1(i)) - coef_vs*sin(-gamma_1(i))
            mat_force_element(1,6) = sin(gamma_1(i))
            mat_force_element(2,1) = - sin(-gamma_1(i))      ! + coef_vhs*cos(gamma_1(i))
            mat_force_element(2,2) = sin(-gamma_1(i)) - coef_vs*cos(gamma_1(i))
            mat_force_element(2,3) = - sin(-gamma_1(i)) - coef_vs*cos(gamma_1(i))
            mat_force_element(2,6) = cos(gamma_1(i))
            mat_force_element(3,2) = (0.5*coef_vs*w_v_ro* - (r_vh + l_v(i) - (l_v_ro - 2.0)))*1.e-3       ! account for 2.0 mm radial tip
            !mat_force_element(3,2) = (0.5*coef_vs*w_v_ro* - (r_vh + l_v(i) - l_v_ro))*1.e-3     ! normal
            mat_force_element(3,3) = (- 0.5*coef_vs*w_v_ro + (r_vh + l_vh))*1.e-3
            mat_force_element(4,2) = cos(gamma_1(i)) + coef_vs*sin(-gamma_1(i))
            mat_force_element(4,3) = - cos(gamma_1(i)) + coef_vs*sin(-gamma_1(i))
            mat_force_element(4,4) = 1.0
            mat_force_element(5,2) = - sin(-gamma_1(i)) + coef_vs*cos(gamma_1(i))
            mat_force_element(5,3) = sin(-gamma_1(i)) + coef_vs*cos(gamma_1(i))
            mat_force_element(5,5) = 1.0
            mat_force_element(6,2) = (r_vh + l_v(i) - (l_v_ro - 2.0) - 0.5*coef_vs*w_v_ro)*1.e-3    ! F1n ! account for 2.0 mm radial tip
            !mat_force_element(6,2) = (r_vh + l_v(i) - l_v_ro - 0.5*coef_vs*w_v_ro)*1.e-3    ! F1n ! normal
            mat_force_element(6,3) = - (r_vh + l_vh - 0.5*coef_vs*w_v_ro)*1.e-3             ! F2n
            !mat_force_element(6,4) = (r_vh + r_oc)*1.e-3
            mat_force_element(6,4) = (l_v(i) + r_vh + r_ro)*1.e-3*cos(gamma_1(i))       ! F_Rx
            mat_force_element(6,5) = -(l_v(i) + r_vh + r_ro)*1.e-3*sin(-gamma_1(i))     ! F_Ry : because after theta_1 > 180 degree, the convention change, so add "-ve" in front gamma_1
        
            ! ---- matrix B ----- !
            mat_B(1) = F_vvh(i)*cos(- gamma_1(i)) - F_vsro_1*sin(-gamma_1(i)) - F_vsro_2*sin(-gamma_1(i)) - F_slot_vh*sin(-gamma_1(i))       ! gamma_1 correted to be -ve
            mat_B(2) = - F_vvh(i)*sin( - gamma_1(i))  - F_vsro_1*cos(-gamma_1(i)) - F_vsro_2*cos(-gamma_1(i)) - F_slot_vh*cos(-gamma_1(i))
            if (l_v(i) > l_vh) then
                mat_B(3) = I_vh_O*dgammadtt_1(i) + F_vvh(i)*(r_vh + 0.5*(l_vh + l_exposed_vh))*1.e-3 + F_vsro_1*0.5*(w_v_ro + w_vs_ro)*1.e-3 - F_vsro_2*0.5*(w_v_ro - w_vs_ro)*1.e-3
                mat_B(6) = I_ro_O(i)*dgammadtt_1(i) + F_AB(i)*(r_vh + l_v(i) + r_ro)*1.e-3*sin(0.5*theta_2(i)) + F_v(i)*(r_vh + l_vh + 0.5*(l_v(i) - l_vh))*1.e-3
            else
                mat_B(3) = I_vh_O*dgammadtt_1(i) + F_vvh(i)*(r_vh + 0.5*(l_v(i) + l_exposed_vh))*1.e-3 + F_vsro_1*0.5*(w_v_ro + w_vs_ro)*1.e-3 - F_vsro_2*0.5*(w_v_ro - w_vs_ro)*1.e-3
                mat_B(6) = I_ro_O(i)*dgammadtt_1(i) + F_AB(i)*(r_vh + l_v(i) + r_ro)*1.e-3*sin(0.5*theta_2(i))
            endif
            mat_B(4) = F_AB(i)*sin(0.5*theta_2(i) - ( - gamma_1(i))) + F_v(i)*cos(gamma_1(i)) + F_slot_vh*sin(-gamma_1(i))         ! case 1
            mat_B(5) = - F_AB(i)*cos(0.5*theta_2(i) - ( - gamma_1(i))) - F_v(i)*sin(- gamma_1(i)) + F_slot_vh*cos(-gamma_1(i))       ! case 1
            !mat_B(4) = F_AB(i)*sin(0.5*pi - gamma_1(i))
            !mat_B(5) = - F_AB(i)*cos(0.5*pi - gamma_1(i))
            !mat_B(7) = F_v(i)*cos(gamma_1(i)) - F_AB(i)*sin(gamma_1(i) - 0.5*theta_2(i))
            !mat_B(8) = - F_v(i)*sin(gamma_1(i)) - F_AB(i)*cos(gamma_1(i) - 0.5*theta_2(i))
            !mat_B(9) = I_ro_O(i)*dgammadtt_1(i) + F_AB(i)*(r_vh + l_v(i) + r_ro)*1.e-3*sin(0.5*theta_2(i)) + F_v(i)*(r_vh + 0.5*l_v(i))*1.e-3
        
            ! ---------------------------------
            ! Bring calcualted coefficients into Gauss Elimination Method
            ! ---------------------------------
            call Gauss_elimination(mat_force_element, mat_B, sol_X, n)

            ! -------------- for solusion version 2-1 - version 2-3
            F_vhoc_n(i) = sol_X(1)      ! [N] resultant force in normal direction acting on vane housing by outer cylinder at point O
            F_vhoc_t(i) = sol_X(6)      ! [N] resultant force in tangential direction acting on vane housing by outer cylinder at point O
            F_1_n(i) = sol_X(2)         ! [N] Normal force acting on vane housing vane at point 1
            F_2_n(i) = sol_X(3)         ! [N] Normal force acting on vane housing vane at point 2
            !sol_X(3) = T_vhoc(i)       ! [Nm] frictional torque due to rubbing surface between vane housing and housing slot of the outer cylinder at point O
            F_rx(i) = sol_X(4)          ! [N] Resultant force in x_direction at point C
            F_ry(i) = sol_X(5)          ! [N] Resultant force in y_direciton at point C
            !T_roll(i) = sol_X(6)       ! [Nm] Torque acting on the rotor to drive the inertia
            F_cx(i) = F_rx(i)   ! force balance of roller body
            F_cy(i) = F_ry(i)   ! force balance of roller body
            !F_resultant(i) = sqrt(F_cx(i)**2 + F_cy(i)**2)     ! without dynamic balance
            F_resultant(i) = sqrt(F_cx(i)**2 + F_cy(i)**2) ! + (0.193*0.01764 - 0.5908*0.00437 - 0.712*0.0011007)*omega_1**2    ! dynamic balance, F = mrw**2
            ! --------------------------------------
            ! Individual components of roller torque
            ! --------------------------------------
            T_inertia_ro(i) = I_ro_O(i)*dgammadtt_1(i)  ! rotor inertia torque about O (Torque reaction is in opposite direction)
            T_inertia_vh(i) = I_vh_O*dgammadtt_1(i)     ! vh inertia torque about O
            ! ------------------------------------
            ! Case 1: F_AB and F_v together about O
            ! Forces exert on rotor only
            ! ------------------------------------
            !T_com_O(i) = F_AB(i)*(r_vh + l_v(i) + r_ro)*1.e-3*sin(0.5*theta_2(i)) + F_v(i)*(r_vh + l_vh + 0.5*(l_v(i) - l_vh))*1.e-3 !- F_cx(i)*(r_oc + r_vh)*1.e-3
            ! ------------------------------------
            ! Case 2: F_AB acts on roller center, creating torque about point C
            ! ------------------------------------
            T_com_C(i) = - (F_rx(i)*e*1.e-3*cos(theta_1(i)) + F_ry(i)*e*1.e-3*sin(theta_1(i)))      ! if resultant force at R
            !T_com_C(i) = F_cx(i)*e*1.e-3*sin(0.5*theta_2(i)) !- F_vhoc_n(i)*(r_oc + r_vh)*1.e-3 - F_AB(i)*e*1.e-3*sin(0.5*theta_2(i)) + F_v(i)*(r_oc + r_vh - (r_vh + l_vh + 0.5*(l_v(i) - l_vh))*cos(gamma_1(i)))*1.e-3 
            !if (l_v(i) > l_vh) then
            !    T_com_C(i) = F_v(i)*cos(gamma_1(i))*(l_OC - ((r_vh + l_vh + 0.5*(l_v(i) - l_vh))*cos(gamma_1(i))))*1.e-3 + F_vvh(i)*cos(gamma_1(i))*(l_OC - (r_vh + 0.5*l_vh)*cos(gamma_1(i)))*1.e-3! +  F_AB(i)*e*sin(0.5*theta_2(i))*1.e-3         ! compression torque about C
            !else
            !    T_com_C(i) = F_vvh(i)*cos(gamma_1(i))*(l_OC - r_vh - 0.5*l_v(i)*cos(gamma_1(i)))*1.e-3 !+ F_AB(i)*e*sin(0.5*theta_2(i))*1.e-3 
            !endif
            !if (F_1_n(i) < 0 .or. F_2_n(i) < 0) then
            !    tilt_case = 2
            !    print*, i, " Negative normal force detected, switch to case ", tilt_case, "..."
            !    goto 168
            !endif
            !T_total_no_loss(i) = T_inertia_ro(i) + T_inertia_ro(i) + T_com_C(i)       ! Total torque (rotor inertia and compression) without loss
!        elseif (tilt_case == 2) then
!168         continue
!            mat_force_element(1,1) = cos(gamma_1(i))        ! + coef_vhs*sin(gamma_1(i))       ! A11
!            mat_force_element(1,2) = - I_vh_O*dgammadtt_1(i)/((r_vh + l_v(i) - l_v_ro)*1.d-3)*cos(gamma_1(i)) !- cos(gamma_1(i)) - coef_vs*sin(-gamma_1(i))        !   A12
!            mat_force_element(1,3) = cos(gamma_1(i)) - coef_vs*sin(-gamma_1(i))
!            mat_force_element(1,6) = sin(gamma_1(i))
!            mat_force_element(2,1) = - sin(-gamma_1(i))      ! + coef_vhs*cos(gamma_1(i))
!            mat_force_element(2,2) = - I_vh_O*dgammadtt_1(i)/((r_vh + l_v(i) - l_v_ro)*1.d-3)*(sin(-gamma_1(i)))!sin(-gamma_1(i)) - coef_vs*cos(gamma_1(i))
!            mat_force_element(2,3) = - sin(-gamma_1(i)) - coef_vs*cos(gamma_1(i))
!            mat_force_element(2,6) = cos(gamma_1(i))
!            mat_force_element(3,2) = - I_vh_O*dgammadtt_1(i) !(0.5*coef_vs*w_v_ro* - (r_vh + l_v(i) - l_v_ro))*1.e-3
!            mat_force_element(3,3) = (- 0.5*coef_vs*w_v_ro + (r_vh + l_vh))*1.e-3
!            mat_force_element(4,2) = - I_ro_O(i)*dgammadtt_1(i)/((r_vh + l_v(i) - l_v_ro)*1.d-3)*cos(gamma_1(i)) !cos(gamma_1(i)) + coef_vs*sin(-gamma_1(i))
!            mat_force_element(4,3) = - cos(gamma_1(i)) + coef_vs*sin(-gamma_1(i))
!            mat_force_element(4,4) = 1.0
!            mat_force_element(5,2) = - I_ro_O(i)*dgammadtt_1(i)/((r_vh + l_v(i) - l_v_ro)*1.d-3)*(-sin(-gamma_1(i))) !- sin(-gamma_1(i)) + coef_vs*cos(gamma_1(i))
!            mat_force_element(5,3) = sin(-gamma_1(i)) + coef_vs*cos(gamma_1(i))
!            mat_force_element(5,5) = 1.0
!            mat_force_element(6,2) = -I_ro_O(i)*dgammadtt_1(i)!0.0!(r_vh + l_v(i) - l_v_ro - 0.5*coef_vs*w_v_ro)*1.e-3    ! F1n
!            mat_force_element(6,3) = - (r_vh + l_vh - 0.5*coef_vs*w_v_ro)*1.e-3             ! F2n
!            !mat_force_element(6,4) = (r_vh + r_oc)*1.e-3
!            mat_force_element(6,4) = (l_v(i) + r_vh + r_ro)*1.e-3*cos(gamma_1(i))       ! F_Rx
!            mat_force_element(6,5) = -(l_v(i) + r_vh + r_ro)*1.e-3*sin(-gamma_1(i))     ! F_Ry : because after theta_1 > 180 degree, the convention change, so add "-ve" in front gamma_1
!        
!            ! ---- matrix B ----- !
!            mat_B(1) = F_vvh(i)*cos(- gamma_1(i)) - F_vsro_1*sin(-gamma_1(i)) - F_vsro_2*sin(-gamma_1(i)) - F_slot_vh*sin(-gamma_1(i))       ! gamma_1 correted to be -ve
!            mat_B(2) = - F_vvh(i)*sin( - gamma_1(i))  - F_vsro_1*cos(-gamma_1(i)) - F_vsro_2*cos(-gamma_1(i)) - F_slot_vh*cos(-gamma_1(i))
!            if (l_v(i) > l_vh) then
!                mat_B(3) = I_vh_O*dgammadtt_1(i) + F_vvh(i)*(r_vh + 0.5*(l_vh + l_exposed_vh))*1.e-3 + F_vsro_1*0.5*(w_v_ro + w_vs_ro)*1.e-3 - F_vsro_2*0.5*(w_v_ro - w_vs_ro)*1.e-3
!                mat_B(6) = I_ro_O(i)*dgammadtt_1(i) + F_AB(i)*(r_vh + l_v(i) + r_ro)*1.e-3*sin(0.5*theta_2(i)) + F_v(i)*(r_vh + l_vh + 0.5*(l_v(i) - l_vh))*1.e-3
!            else
!                mat_B(3) = I_vh_O*dgammadtt_1(i) + F_vvh(i)*(r_vh + 0.5*(l_v(i) + l_exposed_vh))*1.e-3 + F_vsro_1*0.5*(w_v_ro + w_vs_ro)*1.e-3 - F_vsro_2*0.5*(w_v_ro - w_vs_ro)*1.e-3
!                mat_B(6) = I_ro_O(i)*dgammadtt_1(i) + F_AB(i)*(r_vh + l_v(i) + r_ro)*1.e-3*sin(0.5*theta_2(i))
!            endif
!            mat_B(4) = F_AB(i)*sin(0.5*theta_2(i) - ( - gamma_1(i))) + F_v(i)*cos(gamma_1(i)) + F_slot_vh*sin(-gamma_1(i))         ! case 1
!            mat_B(5) = - F_AB(i)*cos(0.5*theta_2(i) - ( - gamma_1(i))) - F_v(i)*sin(- gamma_1(i)) + F_slot_vh*cos(-gamma_1(i))       ! case 1
!        
!            ! ---------------------------------
!            ! Bring calcualted coefficients into Gauss Elimination Method
!            ! ---------------------------------
!            call Gauss_elimination(mat_force_element, mat_B, sol_X, n)
!
!            ! -------------- for solusion version 2-1 - version 2-3
!            F_vhoc_n(i) = sol_X(1)      ! [N] resultant force in normal direction acting on vane housing by outer cylinder at point O
!            F_vhoc_t(i) = sol_X(6)      ! [N] resultant force in tangential direction acting on vane housing by outer cylinder at point O
!            F_1_n(i) = sol_X(2)         ! [N] Normal force acting on vane housing vane at point 1
!            F_2_n(i) = sol_X(3)         ! [N] Normal force acting on vane housing vane at point 2
!            !sol_X(3) = T_vhoc(i)       ! [Nm] frictional torque due to rubbing surface between vane housing and housing slot of the outer cylinder at point O
!            F_rx(i) = sol_X(4)          ! [N] Resultant force in x_direction at point C
!            F_ry(i) = sol_X(5)          ! [N] Resultant force in y_direciton at point C
!            !T_roll(i) = sol_X(6)       ! [Nm] Torque acting on the rotor to drive the inertia
!            F_cx(i) = F_rx(i)   ! force balance of roller body
!            F_cy(i) = F_ry(i)   ! force balance of roller body
!            !F_resultant(i) = sqrt(F_cx(i)**2 + F_cy(i)**2)     ! without dynamic balance
!            F_resultant(i) = sqrt(F_cx(i)**2 + F_cy(i)**2)  + (0.193*0.01764 - 0.5908*0.00437 - 0.712*0.0011007)*omega_1**2    ! dynamic balance, F = mrw**2
!            ! --------------------------------------
!            ! Individual components of roller torque
!            ! --------------------------------------
!            T_inertia_ro(i) = I_ro_O(i)*dgammadtt_1(i)  ! rotor inertia torque about O (Torque reaction is in opposite direction)
!            T_inertia_vh(i) = I_vh_O*dgammadtt_1(i)     ! vh inertia torque about O
!            ! ------------------------------------
!            ! Case 1: F_AB and F_v together about O
!            ! Forces exert on rotor only
!            ! ------------------------------------
!            ! T_com_O(i) = F_AB(i)*(r_vh + l_v(i) + r_ro)*1.e-3*sin(0.5*theta_2(i)) + F_v(i)*(r_vh + l_vh + 0.5*(l_v(i) - l_vh))*1.e-3 !- F_cx(i)*(r_oc + r_vh)*1.e-3
!            
!            ! ------------------------------------
!            ! Case 2: F_AB acts on roller center, creating torque about point C
!            ! ------------------------------------
!            T_com_C(i) = - (F_rx(i)*e*1.e-3*cos(theta_1(i)) + F_ry(i)*e*1.e-3*sin(theta_1(i)))      ! if resultant force at R
!            if (F_1_n(i) < 0 .or. F_2_n(i) < 0) then
!                tilt_case = 1
!                print*, i, " Negative normal force detected, switch to case ", tilt_case, "..."
!                goto 167
!            endif
!        endif
123 continue
    print *, ' ---------------------------------------------------------------------------- '  
    write(6,2989) " Maximum F_cx              = ", maxval(F_cx), " N"
    write(6,2989) " Maximum F_cy              = ", maxval(F_cy), " N"
    write(6,2989) " Maximum Resultant Load    = ", sqrt(maxval(F_cx)**2 + maxval(F_cy)**2), " N"
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' '

    
    ! Data writing is in the next model "power_m.f90"
     


2981 format (14A25)
2982 format (F25.4, 14ES25.6E2)    
2983 format (14ES25.6E2) 
2989 format (2x,A,F12.4,A)    
     
    endsubroutine dynamic_m2
    
    
    
    
    
    ! -------------------------------------------------------
    ! Yanagisawa 1982 Coefficient of Friction at vane tip, RP
    ! -------------------------------------------------------
    ! coef_vtip = 0.15 - 35*sqrt(miu_oil*l_com2*dgammadt_2(i)*(0.5*w_vane2)*1.e-6/F_n_v2(i))
    
    ! --------------------------------------------------------
    ! This part is for version 1-0 where there is 9 unknowns and 9 equations
    ! failed on 21-11-2017
    ! --------------------------------------------------------
    !mat_force_element(1,1) = 1.0        ! A11
    !    mat_force_element(1,5) = - (cos(gamma_1(i)) + coef_vs*sin(gamma_1(i)))        !   A15
    !    mat_force_element(1,6) = cos(gamma_1(i)) - coef_vs*sin(gamma_1(i))
    !    mat_force_element(2,2) = 1.0
    !    mat_force_element(2,5) = sin(gamma_1(i)) - coef_vs*cos(gamma_1(i))
    !    mat_force_element(2,6) = - coef_vs*cos(gamma_1(i)) - sin(gamma_1(i))
    !    mat_force_element(3,3) = 1.0
    !    mat_force_element(3,4) = - 1.0
    !    mat_force_element(3,5) = (0.5*coef_vs*w_v_ro* - (r_vh + l_v(i) - l_v_ro))*1.e-3
    !    mat_force_element(3,6) = (- 0.5*coef_vs*w_v_ro + (r_vh + l_vh))*1.e-3
    !    mat_force_element(4,5) = cos(gamma_1(i)) + coef_vs*sin(gamma_1(i))
    !    mat_force_element(4,6) = - cos(gamma_1(i)) + coef_vs*sin(gamma_1(i))
    !    mat_force_element(4,7) = 1.0
    !    mat_force_element(5,5) = - sin(gamma_1(i)) + coef_vs*cos(gamma_1(i))
    !    mat_force_element(5,6) = sin(gamma_1(i)) + coef_vs*cos(gamma_1(i))
    !    mat_force_element(5,8) = 1.0
    !    mat_force_element(6,3) = - 1.0
    !    mat_force_element(6,5) = (r_vh + l_v(i) - l_v_ro - 0.5*coef_vs*w_v_ro)*1.e-3
    !    mat_force_element(6,6) = - (r_vh + l_vh - 0.5*coef_vs*w_v_ro)*1.e-3
    !    mat_force_element(6,7) = (r_vh + r_oc)*1.e-3
    !    mat_force_element(6,9) = 1.0
    !    mat_force_element(7,1) = 1.0
    !    mat_force_element(7,7) = 1.0
    !    mat_force_element(8,2) = 1.0
    !    mat_force_element(8,8) = 1.0
    !    mat_force_element(9,4) = - 1.0
    !    mat_force_element(9,7) = (r_vh + r_oc)*1.e-3
    !    mat_force_element(9,9) = 1.0
    
    !sol_X(1) = F_ox(i)      ! [N] resultant force in x-direction acting on vane housing by outer cylinder at point O
    !    sol_X(2) = F_oy(i)      ! [N] resultant force in y-direction acting on vane housing by outer cylinder at point O
    !    sol_X(3) = T_vhro(i)    ! [Nm] torque acting on vh by rotor
    !    sol_X(4) = M_o(i)          ! [Nm] resultant moment acting on vane housing at point O
    !    sol_X(5) = F_1_n(i)        ! [N] normal force acting on vane of the vane housing at point 1
    !    sol_X(6) = F_2_n(i)        ! [N] normal force acting on vane of the vane housing at point 2
    !    sol_X(7) = F_cx(i)      ! [N] Resultant force in x_direction at point C
    !    sol_X(8) = F_cy(i)      ! [N] Resultant force in y_direciton at point C
    !    sol_X(9) = T_roll(i)    ! [Nm] Resultant torque acting on the roller (torque required to drive the compressor)
    ! ------------------------version 2-1 ----------------------------
    !mat_force_element(1,1) = cos(gamma_1(i)) + coef_vhs*sin(gamma_1(i))       ! A11
        !mat_force_element(1,2) = - cos(gamma_1(i)) - coef_vs*sin(gamma_1(i))        !   A12
        !mat_force_element(1,3) = cos(gamma_1(i)) + coef_vs*sin(gamma_1(i))
        !mat_force_element(2,1) = - sin(gamma_1(i)) + coef_vhs*cos(gamma_1(i))
        !mat_force_element(2,2) = sin(gamma_1(i)) - coef_vs*cos(gamma_1(i))
        !mat_force_element(2,3) = - sin(gamma_1(i)) + coef_vs*cos(gamma_1(i))
        !mat_force_element(3,2) = (0.5*coef_vs*w_v_ro* - (r_vh + l_v(i) - l_v_ro))*1.e-3
        !mat_force_element(3,3) = (0.5*coef_vs*w_v_ro + (r_vh + l_vh))*1.e-3
        !mat_force_element(4,2) = cos(gamma_1(i)) + coef_vs*sin(gamma_1(i))
        !mat_force_element(4,3) = - cos(gamma_1(i)) - coef_vs*sin(gamma_1(i))
        !mat_force_element(4,4) = 1.0
        !mat_force_element(5,2) = - sin(gamma_1(i)) + coef_vs*cos(gamma_1(i))
        !mat_force_element(5,3) = sin(gamma_1(i)) - coef_vs*cos(gamma_1(i))
        !mat_force_element(5,5) = 1.0
        !mat_force_element(6,2) = (r_vh + l_v(i) - l_v_ro - 0.5*coef_vs*w_v_ro)*1.e-3
        !mat_force_element(6,3) = - (r_vh + l_vh + 0.5*coef_vs*w_v_ro)*1.e-3
        !mat_force_element(6,4) = (r_vh + r_oc)*1.e-3
        !mat_force_element(6,6) = 1.0
    ! ---------------------- version 2-2 --------------------------
            !mat_force_element(1,1) = - cos(gamma_1(i)) - coef_vs*sin(gamma_1(i))        !   A12
        !mat_force_element(1,2) = cos(gamma_1(i)) + coef_vs*sin(gamma_1(i))
        !mat_force_element(2,1) = sin(gamma_1(i)) - coef_vs*cos(gamma_1(i))
        !mat_force_element(2,2) = - sin(gamma_1(i)) + coef_vs*cos(gamma_1(i))
        !mat_force_element(3,1) = (0.5*coef_vs*w_v_ro* - (r_vh + l_v(i) - l_v_ro))*1.e-3
        !mat_force_element(3,2) = (0.5*coef_vs*w_v_ro + (r_vh + l_vh))*1.e-3
        !mat_force_element(3,3) = - 1.0
        !mat_force_element(4,1) = cos(gamma_1(i)) + coef_vs*sin(gamma_1(i))
        !mat_force_element(4,2) = - cos(gamma_1(i)) - coef_vs*sin(gamma_1(i))
        !mat_force_element(4,4) = 1.0
        !mat_force_element(5,1) = - sin(gamma_1(i)) + coef_vs*cos(gamma_1(i))
        !mat_force_element(5,2) = sin(gamma_1(i)) - coef_vs*cos(gamma_1(i))
        !mat_force_element(5,5) = 1.0
        !mat_force_element(6,1) = (r_vh + l_v(i) - l_v_ro - 0.5*coef_vs*w_v_ro)*1.e-3
        !mat_force_element(6,2) = - (r_vh + l_vh + 0.5*coef_vs*w_v_ro)*1.e-3
        !mat_force_element(6,4) = (r_vh + r_oc)*1.e-3
        !mat_force_element(6,6) = 1.0
    
    ! -------------- for solusion version 2-1 and version 2-2
    !sol_X(1) = F_vhoc_n(i)      ! [N] resultant force in normal direction acting on vane housing by outer cylinder at point O
    !sol_X(1) = F_1_n(i)     ! [N] Normal force acting on vane housing vane at point 1
    !sol_X(2) = F_2_n(i)     ! [N] Normal force acting on vane housing vane at point 2
    !sol_X(3) = T_vhoc(i)    ! [Nm] frictional torque due to rubbing surface between vane housing and housing slot of the outer cylinder at point O
    !sol_X(4) = F_cx(i)      ! [N] Resultant force in x_direction at point C
    !sol_X(5) = F_cy(i)      ! [N] Resultant force in y_direciton at point C
    !sol_X(6) = T_roll(i)    ! [Nm] Resultant torque acting on the roller (torque required to drive the compressor)
    
    
    ! ----------------------------------------------
    ! this section is used to smoothen "infinity" or "NaN"
    ! ----------------------------------------------
    !do 125 i = 1, no_data+1
        !if (i == no_data/2) then
        !    T_total_no_loss(i) = 0.5* (T_total_no_loss(i-1) + T_total_no_loss(i+1))
        !elseif (i == no_data) then
        !    T_total_no_loss(i) = 0.5* (T_total_no_loss(i-1) + T_total_no_loss(i+1))
        !endif
        !write(100,2983) T_total_no_loss(i)
!125 continue  