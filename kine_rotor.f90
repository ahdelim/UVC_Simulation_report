subroutine kine_rotor(theta_1, gamma_1, l_v, dlvdt, dlvdtt, dgammadt_1, dgammadtt_1, dlvdtheta1, dlvdtheta12, dtheta2dt)
    
    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    include "var_operational_parameter.f90"
    include "var_simulation_parameter.f90"
    include "var_kinematics.f90"
    include "var_physical_constants.f90"
    integer i
    ! ------ Dummy Variables ------
    double precision termA, termB, termC, termD, termE
    double precision term_gammaA, term_gammaB, term_gammaC
    double precision term_difgammaAlv, term_difgammaClv,dgamma1dlv2
    
    
    do 452 i = 1, no_data+1
        
        termA = 1.0 - (((r_oc + r_vh)**2 - e**2 - (r_ro + l_v(i) + r_vh)**2)/(2.*e*(r_ro + l_v(i) + r_vh)))**2    ! theta 2
        termB = 1.0/e + ((r_oc + r_vh)**2 - e**2 - (r_ro + l_v(i) + r_vh)**2)/(2.*e*(r_ro + l_v(i) + r_vh)**2)    ! theta 2
        term_gammaB = ((r_oc + r_vh)**2 - e**2 + (r_ro + l_v(i) + r_vh)**2)/(2.*(r_oc + r_vh)*(r_ro + l_v(i) + r_vh))      ! gamma, B
        term_gammaC = - 1.0/sqrt(1.0 - term_gammaB**2)      ! gamma, C
        term_gammaA = 1.0/(r_oc + r_vh) - ((r_oc + r_vh)**2 - e**2 + (r_ro + l_v(i) + r_vh)**2)/(2.*(r_oc + r_vh)*(r_ro + l_v(i) + r_vh)**2)            ! gamma, A
        
        term_diff_A = 1./((r_vh + r_oc)*(r_vh + l_v(i) + r_ro))
        term_diff_B = (r_oc + r_vh)**2 - e**2 + (r_ro + l_v(i) + r_vh)**2
        term_diff_C = (r_vh + r_oc)*(r_vh + l_v(i) + r_ro)**3
        
        if (theta_1(i)*180.0/pi < 180.0) then
            dgammadt_1(i) = - term_gammaA*term_gammaC*dlvdt(i)     ! vane housing/rotor rotational speed [rad/s]
        else
            dgammadt_1(i) = term_gammaA*term_gammaC*dlvdt(i)
        endif
        
        if (theta_1(i)*180.0/pi < 180.0) then
            dgammadtheta1_1(i) = - dlvdtheta1(i)*term_gammaA*term_gammaC
        else
            dgammadtheta1_1(i) = dlvdtheta1(i)*term_gammaA*term_gammaC
        endif
        
        if (theta_1(i)*180.0/pi < 180.0) then
            dgammadtt_1(i) =  - (term_gammaC*(term_diff_A - term_diff_B/term_diff_C) - 1.0/(1.0 - term_gammaB**2)**1.5*term_gammaB*(-term_gammaA)**2)*dlvdt(i)**2 + (term_gammaC*(-term_gammaA))*omega_1**2*dlvdtheta12(i)       ! Vane housing/Rotor acceleration [rad/s2]
        else
            dgammadtt_1(i) = ( (term_gammaC*(term_diff_A - term_diff_B/term_diff_C) - 1.0/(1.0 - term_gammaB**2)**1.5*term_gammaB*(-term_gammaA)**2)*dlvdt(i)**2 + (term_gammaC*(-term_gammaA))*omega_1**2*dlvdtheta12(i))       ! Vane housing/Rotor acceleration [rad/s2]
            !dgammadtt_1(i) = - dgamma1dlv2*dlvdt(i)**2 + dgamma1dlv*dlvdtt(i)            ! vane housing/rotor acceleration [rad/s2]
        endif

        if (theta_1(i)*180.0/pi < 180.0) then
            dtheta2dt(i) = 1.0/sqrt(termA) * termB * dlvdtheta1(i) * omega_1
        else
            dtheta2dt(i) = - 1.0/sqrt(termA) * termB * dlvdtheta1(i) * omega_1
        endif
     
        
452 continue
    ! --------------------------------
    ! data correction for "infinity" and "NaN"
    ! --------------------------------
    do 453 i=1, no_data+1
        if (i == no_data/2+1) then
            dgammadtheta1_1(i) = 0.5*(dgammadtheta1_1(i-1) + dgammadtheta1_1(i+1))
            !dgammadt_1(i) = 0.5*(dgammadt_1(i-1) + dgammadt_1(i+1))
            dgammadt_1(i) = dgammadt_1(i-1) + (dgammadt_1(i-2) - dgammadt_1(i-1))
            dtheta2dt(i) = dtheta2dt(i-1) + (dtheta2dt(i-2) - dtheta2dt(i-1))
            !dgammadtt_1(i) = 0.5*(dgammadtt_1(i-1) + dgammadtt_1(i+1))
        elseif (i == no_data) then
            dgammadtheta1_1(i) = dgammadtheta1_1(i-1) + (dgammadtheta1_1(i-2) - dgammadtheta1_1(i-1))
            dgammadt_1(i) = dgammadt_1(i-1) + (dgammadt_1(i-2) - dgammadt_1(i-1))
            dtheta2dt(i) = dtheta2dt(i-1) + (dtheta2dt(i-2) - dtheta2dt(i-1))
        elseif (i == 1) then
            dgammadtheta1_1(i) = dgammadtheta1_1(i+1) - (dgammadtheta1_1(i+2) - dgammadtheta1_1(i+1))
            dgammadt_1(i) = dgammadt_1(i+1) - (dgammadt_1(i+2) - dgammadt_1(i+1))
            dtheta2dt(i) = dtheta2dt(i+1) - (dtheta2dt(i+2) - dtheta2dt(i+1))
            !dgammadtt_1(i) = dgammadtt_1(i+1)
        elseif (i == no_data+1) then
            dgammadtheta1_1(i) = dgammadtheta1_1(i-1) + (dgammadtheta1_1(i-2) - dgammadtheta1_1(i-1))
            dgammadt_1(i) = dgammadt_1(i-1) + (dgammadt_1(i-2) - dgammadt_1(i-1))
            dtheta2dt(i) = dtheta2dt(i-1) + (dtheta2dt(i-2) - dtheta2dt(i-1))
            !dgammadtt_1(i) = dgammadtt_1(i-1)
        endif
        
        
453 continue
    
    
    
    do 454 i=1,no_data+1
        if (i == no_data+1) then
            dgammadtt_1(i) = (dgammadt_1(i) - dgammadt_1(i-1))/(theta_1(i) - theta_1(i-1))*omega_1
        else
            dgammadtt_1(i) = (dgammadt_1(i+1) - dgammadt_1(i))/(theta_1(i+1) - theta_1(i))*omega_1
        endif
454 continue
    
    
endsubroutine kine_rotor