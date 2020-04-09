subroutine journal_mecc_matt(ecc_ratio, att_angle, theta_position_1, k_convention, l_bear, r_bear, M_ecc, M_att)
    ! -------------------------------
    ! this routine calculate M_ecc and M_att
    ! of Hirani Journal Bearing model
    ! Weddle's Formula implemented for integration
    ! -------------------------------
    implicit none
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    ! -------- For simulation --------------
    integer i, j, n
    ! ----------- Input variables ------------
    double precision ecc_ratio, att_angle, theta_position_1
    double precision L_b, R_b, l_bear, r_bear
    ! ----------- Output variables -----------
    double precision M_ecc, M_att
    ! ----------- Dummy variables ------------
    double precision I1, I2, I3, B1, B2, H
    double precision h_step, theta_dummy, k_convention
    
    ! ----------------------------------
    ! Initialisation
    ! ----------------------------------
    L_b = l_bear
    R_b = r_bear     
    h_step = pi/36.     ! 180 is divided into 6 sections, each section with 30 degree, and 30 degree divided by 6 again (b-a)/6, thus total 36
    I1 = 0.0
    I2 = 0.0
    I3 = 0.0
    theta_dummy = theta_position_1
    H = 0.0
    do j = 1,6
        do i = 1,7
		    H = 1.0 + ecc_ratio*cos(theta_dummy)        ! Nondimensional film thickness
		    B1 = H*(1. + H)/(L_b/R_b)**2
		    B2 = (1. - (B1/sqrt(B1 + 0.25))*log((0.5 + sqrt(B1 + 0.25))/(-0.5 + sqrt(B1 + 0.25))))*((1. + H)/H**2)
            if (i == 1 .or. i == 3 .or. i == 5 .or. i == 7) then
			    n = 1
            elseif (i == 2 .or. i == 6) then
			    n = 5
            elseif (i == 4) then
			    n = 6
            endif
		    I1 = I1 + 6.*h_step/20.*n*B2*cos(theta_dummy)**2
		    I2 = I2 + 6.*h_step/20.*n*B2*cos(theta_dummy)*sin(theta_dummy)
		    I3 = I3 + 6.*h_step/20.*n*B2*sin(theta_dummy)**2
            if (i < 7) then
                theta_dummy = theta_dummy + k_convention*h_step
            endif 
        enddo
    enddo

    M_ecc = (I3*cos(att_angle) + I2*sin(att_angle)) / (I1*I3 - I2*I2)
    M_att = (I1*sin(att_angle) + I2*cos(att_angle)) / (I2*I2 - I1*I3)
    
    
endsubroutine