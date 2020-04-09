subroutine leakage_orifice(omega_1, area_orf, coef_discport, p2, rho1, h1, s1, dmdtheta1_leak_vs)
   
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
    double precision v_sound, v2
    
    ! Thermo variables
    double precision p2, s1, h1, h2s, h2s_mol, h2, rho1, h2_mol
    
    p = p2              ! [kPa]
    s = s1/1000 * wm        ! in [J/mol.K]
    h1_mol = h1/1000 * wm       ! in [J/mol]
    
    call PSFLSH(p,s,z,t,D,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)        ! using Pressure and Entropy to find isentropy H
    !call PSFLSH (p,s,x,tt,dd,dl,dv,xliq,xvap,q,e,h1,cv,cp,w,ierr,herr)
    if (ierr.ne.0) write (102,*) herr           ! write error statement if error occured
    
    h2s_mol = h    ! enthalpy under Isentropy condition h2s (J/mol)
    h2s = h2s_mol*1000/wm       ! [J/kg.K]  convert J/mol to J/kg.K
    v_sound = w     ! Speed of sound in the state [m/s]
    
    ! Condition to calculate v2
    if (h1 > h2s) then
        v2 = sqrt(2.0*(h1 - h2s))     ! [m/s]
    else
        v2 = 0.0
    endif

    ! Discharge coefficient to obtain from Munson - Fundamentals of Fluid Mechanics pg 442 (value range: 0.58-0.65)
    ! Assume discharge coefficient same as discharge port coefficient: 0.61
    
    ! account for choked flow
    if (v2 < abs(v_sound)) then
        dmdtheta1_leak_vs = (coef_discport * area_orf * rho1 * v2)/omega_1          ! [kg/rad]
    else
        dmdtheta1_leak_vs = (coef_discport * area_orf * rho1 * v_sound)/omega_1     ! [kg/rad]
    endif
    
    endsubroutine leakage_orifice