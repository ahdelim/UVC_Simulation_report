subroutine m_flow_disc_ver2(r_oc, theta_1, omega_1, disc_coef, y_mid, p2, rho1, s1, h1, d_port, dmdtheta1_cv, theta_port_start, theta_port_end, area_port_open)
    
    ! --------------------------------------
    ! Mass flow model
    ! dmdt = Cd * density_upstream * sqrt(2*(h_up - h_down)) * Area
    ! --------------------------------------
    
     ! REFPROP
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1, ncmax=20
    dimension z(ncmax),x(ncmax),y(ncmax)
    
    ! Parameter
    include "var_physical_constants.f90"
    common/fluid_info/wm
    
    ! Variables
    double precision omega_1, disc_coef, p2, s1, d_port, h1, h2s, h2s_mol, h2, rho1, v_sound, v2, dmdtheta1_cv, h2_mol
    !double precision y_start, y_end
    double precision y_mid
    double precision theta_port_start, theta_port_end       ! port position parameters
    double precision theta_1, theta_port_dummy                ! rotational angle and dummy angle 
    double precision area_port_open, a_circumference_dv, area_flow       ! real flow area and port circumference area (because valve block it)
    
    ! -------------------------------------
    ! Flow Area (CASE 1)
    ! ** TO THE END OF THE CHAMBER 
    ! due to the position of port
    ! the rotor may cover part of the port during the rotation
    ! causing 3 cases of port flow area --> fully opened, partially opened and fully closed.
    ! --------------------------------------
    if (theta_1 < theta_port_start) then
        area_port_open = 0.25*pi*(d_port/1000.)**2
    elseif (theta_1 >= theta_port_start .and. theta_1 < theta_port_end) then
        theta_port_dummy = acos(1.0 - r_oc*sin(theta_1 - theta_port_start)/(0.5*d_port))
        area_port_open = 0.25*pi*(d_port/1000.)**2 - (0.5*d_port/1000.)**2*(theta_port_dummy - 0.5*sin(2*theta_port_dummy))
    else
        area_port_open = 0.0
    endif
    
    ! ===================================================================
    ! Flow area prototype version (CASE 2)
    ! -------------------------------------------------------------------
    ! In experiment, pressure transducer is placed near the discharge port (not to the end of chamber)
    ! so the chamber is always connected
    ! ****
    ! however circumference area still has to be compared
    ! To compare the circumference area with the port opening area
    ! the smaller one will be the real flow area
    ! -------------------------------------------------------------------
    !area_port_open = 0.25*pi*(d_port/1000.)**2      ! comment out this if to use Case 1
    a_circumference_dv = (d_port/1000.)*pi*y_mid
    
    if (a_circumference_dv < area_port_open) then
        area_flow = a_circumference_dv
    else
        area_flow = area_port_open
    endif
    
    ! ------------------------------------
    ! Discharge mass flow model version 2-0
    ! Upstream flow
    ! Input downstream pressure and upstream entropy to get 
    ! downstream isentropic enthalpy from refprop
    ! ------------------------------------
    p = p2              ! [kPa]
    s = s1/1000. * wm        ! in [J/mol.K]
    h1_mol = h1/1000. * wm       ! in [J/mol]
    call PSFLSH(p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)        ! using Pressure and Entropy to find isentropy H
    !call PSFLSH (p,s,x,tt,dd,dl,dv,xliq,xvap,q,e,h1,cv,cp,w,ierr,herr)
    if (ierr.ne.0) then
        write (105,*) herr 
        !print*, p,s
    endif
    
    h2s_mol = h    ! Isentropy h2s (J/mol)
    h2s = h2s_mol*1000./wm       ! [J/kg.K]
    v_sound = w     ! Speed of sound in [m/s]
    
    ! --------------------------------------
    ! Downstream flow properties
    ! --------------------------------------
    !h2_mol = h1_mol - eff_is*(h1_mol-h2s_mol)   ! real h2 calculation in (J/mol)
    !h = h2_mol
    !call PHFLSH (p,h,z,t,D,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
    !if (ierr.ne.0) then
    !    write (106,*) herr 
    !endif
    !
    !rho2 = D*wm ! [mol/L * g/mol] --> [kg/m^3]
    !
    !h2 = h2_mol*1000./wm     ! [J/kg]
    
    if (y_mid > 0) then
        if (h1 > h2s) then
            v2 = sqrt(2.*(h1-h2s))          ! This formula taken into account the discharge coefficient (isentropic efficiency * loss factor)
        else
            v2 = 0.
        endif
        
        if (v2 < abs(v_sound)) then
            dmdtheta1_cv = (disc_coef * area_flow * rho1 * v2)/omega_1
        else
            dmdtheta1_cv = (disc_coef * area_flow * rho1 * v_sound)/omega_1
        endif
        
    else
        v2 = 0.
        dmdtheta1_cv = 0.
    endif
        
    endsubroutine m_flow_disc_ver2