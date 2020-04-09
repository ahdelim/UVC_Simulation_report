subroutine m_flow_suc_ver2(r_oc, theta_1, omega_1, disc_coef, p2, rho1, s1, h1, d_port, dmdtheta1_cv, theta_port_start, theta_port_end)
    
    ! --------------------------------------
    ! Mass flow model version 2-0
    ! dmdt = Cd * density_upstream * sqrt(2*(h_up - h_down)) * Area
    ! Partially opening area due to the rotational angle is taken into consideration
    ! --------------------------------------
    
    ! ----------------------------------------
    ! REFPROP
    ! Variables declaration based on RefProp
    ! ----------------------------------------
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
    double precision r_oc
    double precision omega_1, disc_coef, d_port, area_port_open, area_flow, v_sound, v2, dmdtheta1_cv       ! variables that bring in from parent routine
    double precision p2, s1, h1, h2s, h2s_mol, h2, rho1, h2_mol                             ! variables bring in and out 
    double precision theta_port_start, theta_port_end                                       ! port position parameters
    double precision theta_1, theta_port_dummy                ! rotational angle and dummy angle 
    
    ! -------------------------------------
    ! Case 1
    ! Flow Cross Section Area
    ! due to the position of port
    ! the rotor may cover part of the port during the rotation
    ! causing 3 cases of port flow area --> fully closed, partially opened and fully opened.
    ! --------------------------------------
    if (theta_1 < theta_port_start) then
        area_port_open = 0.0
    elseif (theta_1 >= theta_port_start .and. theta_1 < theta_port_end) then
        theta_port_dummy = acos(1.0 - r_oc*sin(theta_1 - theta_port_start)/(0.5*d_port))
        area_port_open = 2*(0.5*d_port/1000.)**2*(0.5*theta_port_dummy - 0.5*sin(theta_port_dummy)*cos(theta_port_dummy))
    else
        area_port_open = 0.25*pi*(d_port/1000.0)**2    ! Hole/Port area (m^2)       ! case 2
    endif
        
    ! ------------------------------------
    ! Case 2
    ! If suction port opening area is not considered
    ! ------------------------------------
    !area_port_open = 0.25*pi*(d_port/1000.0)**2
    area_flow = area_port_open
    ! ------------------------------------
    ! Upstream flow
    ! Input downstream pressure and upstream entropy to get 
    ! downstream isentropic enthalpy from refprop
    ! ------------------------------------
    p = p2              ! [kPa]
    s = s1/1000. * wm        ! in [J/mol.K]
    h1_mol = h1/1000. * wm       ! in [J/mol]
  
    call PSFLSH(p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)        ! using Pressure and Entropy to find isentropy H
    !call PSFLSH (p,s,x,tt,dd,dl,dv,xliq,xvap,q,e,h1,cv,cp,w,ierr,herr)
    if (ierr.ne.0) write (102,*) herr           ! write error statement if error occured
    
    h2s_mol = h    ! enthalpy under Isentropy condition h2s (J/mol)
    h2s = h2s_mol*1000./wm       ! [J/kg.K] ! convert J/mol to J/kg.K
    v_sound = w     ! Speed of sound in the state [m/s]
    
    ! -------------------------------------
    ! Downstream flow
    ! calculate real downstream enthalpy based on isentropic efficiency
    ! -------------------------------------
    
    !h2_mol = h1_mol - eff_is*(h1_mol-h2s_mol)   ! real h2 calculation (with isentropic efficiency) in (J/mol)
    !h=h2_mol    ! assign input
    !call PHFLSH (p,h,z,t,D,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
    !if (ierr.ne.0) write (103,*) herr
    !rho2 = D*wm ! [mol/L * g/mol] --> [kg/m^3]
    
    !h2 = h2_mol*1000.0/wm     ! [J/kg]
    
    ! ------------------------------------
    ! Using Bernouli equation/flow energy equation to
    ! calculate downstream speed
    ! -------------------------------------
    
    if (h1 > h2s) then
        v2 = sqrt(2.0*(h1-h2s))
    else
        v2 = 0.0
    endif

    ! account for choked flow
    if (v2 < v_sound) then
        dmdtheta1_cv = (disc_coef * area_flow * rho1 * v2)/omega_1
    else
        dmdtheta1_cv = (disc_coef * area_flow * rho1 * v_sound)/omega_1
    endif
    
end subroutine m_flow_suc_ver2