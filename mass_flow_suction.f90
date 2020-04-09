subroutine m_flow_suc(omega_1, disc_coef, p2, rho1, s1, h1, d_port, dmdtheta1_cv)
    
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
    double precision omega_1, disc_coef, d_port, area_port, v_sound, v2, dmdtheta1_cv
    double precision p2, s1, h1, h2s, h2s_mol, h2, rho1, h2_mol
    
    ! Flow Cross Section Area
    area_port = 0.25*pi*(d_port/1000.0)**2    ! Hole/Port area (m^2)
    
    ! ------------------------------------
    ! Upstream flow
    ! Input downstream pressure and upstream entropy to get 
    ! downstream isentropic enthalpy from refprop
    ! ------------------------------------
    p = p2              ! [kPa]
    s = s1/1000.0 * wm        ! in [J/mol.K]
    h1_mol = h1/1000.0 * wm       ! in [J/mol]
    
    call PSFLSH(p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)        ! using Pressure and Entropy to find isentropy H
    !call PSFLSH (p,s,x,tt,dd,dl,dv,xliq,xvap,q,e,h1,cv,cp,w,ierr,herr)
    if (ierr.ne.0) then 
        write (102,*) herr  ! write error statement if error occured
        !print*, p,s,z,t,D,Dl
        !pause
    endif
    h2s_mol = h    ! enthalpy under Isentropy condition h2s (J/mol)
    h2s = h2s_mol*1000.0/wm       ! [J/kg.K]  convert J/mol to J/kg.K
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
        dmdtheta1_cv = (disc_coef * area_port * rho1 * v2)/omega_1
    else
        dmdtheta1_cv = (disc_coef * area_port * rho1 * v_sound)/omega_1
    endif

end subroutine m_flow_suc