subroutine refprop_rho_h(rho_in, h_in, t_out, p_out, u_out, s_out, cp_out, visc_out, therma_cond_out)
    
    ! REFPROP variables
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1, ncmax = 20
    dimension z(ncmax),x(ncmax),y(ncmax), xliq(ncmax), xvap(ncmax)
    character(255) :: fluid
    
    common/fluid_info/wm
    
    ! ---- define inputs -----
    D = rho_in/wm       ! [kg/m3] / [g/mol] = [g/L] / [g/mol] ---> [mol/L]
    h = h_in*wm/1000.   ! [J/kg]*[g/mol]/1000.0 ---> [J/mol]
    
    call DHFLSH (D,h,z,t,p,Dl,Dv,x,y,q,e,s,cv,cp,w,ierr,herr)
    
    ! ---------- Output properties ---------- 
    t_out = t             ! [K]
    p_out = p             ! [kPa]
    u_out = e/wm*1000.          ! internal energy [J/kg]
    s_out = s/wm*1000.0          ! enthalpy [J/kg]
    cp_out = cp/wm*1000.0        ! specific heat in [J/(kg.K)]
    
    ! -------------------------------------
    ! another routine is used to find viscosity and conductivity
    ! define inputs 
    ! -------------------------------------
    rho = D
    t = t_out
    
    !call TRNECS (t,rho,x,eta,tcx,ierr,herr)
    call TRNPRP (t,rho,x,eta,tcx,ierr,herr)
    visc_out = eta*1.d-6  ! Dynamic Viscosity [Pa.s or N.s/m^2]
    therma_cond_out = tcx    ! Thermal conductivity [W/m.K]
    
endsubroutine refprop_rho_h
    
    
    !temperature                     K
!pressure, fugacity              kPa
!density                         mol/L
!composition                     mole fraction
!quality                         mole basis (moles vapor/total moles)
!enthalpy, internal energy       J/mol
!Gibbs, Helmholtz free energy    J/mol
!entropy, heat capacity          J/(mol.K)
!speed of sound                  m/s
!Joule-Thomson coefficient       K/kPa
!d(p)/d(rho)                     kPa.L/mol
!d2(p)/d(rho)2                   kPa.(L/mol)^2
!viscosity                       microPa.s (10^-6 Pa.s)
!thermal conductivity            W/(m.K)
!dipole moment                   debye
!surface tension                 N/m

!wmm--molecular weight [g/mol]
!c     ttrp--triple point temperature [K]
!c    tnbpt--normal boiling point temperature [K]
!c       tc--critical temperature [K]
!c       pc--critical pressure [kPa]
!c       Dc--critical density [mol/L]
!c       Zc--compressibility at critical point [pc/(Rgas*Tc*Dc)]
!c      acf--acentric factor [-]
!c      dip--dipole moment [debye]
!c     Rgas--gas constant [J/mol-K]