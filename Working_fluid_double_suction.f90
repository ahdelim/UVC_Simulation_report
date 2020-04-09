subroutine working_fluid_double_suction
    
    ! REFPROP variables
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: fluid, herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1

    include "var_operating_fluid_condition.f90"
    common/fluid_info/wm
    t_low = 7.2 + 273.15  ! Suction temperature in Kelvin (from Evaporator)
    ! t_high = 54.4 + 273.15 ! Discharge temperature in Kelvin (to Condenser)

    ! ------------------------------- 1st stage suction condition ----------------------------------- !
    t = t_low
    i = 2
    call SATT(t,x,i,p,dl,dv,xliq,xvap,ierr,herr)
    p_suc1 = p          ! suction pressure of 1st stage compressor = pressure found from refprop
    
    t_suc1 = 35 + 273.15  ! After heat exchanger (Evaporator) at room temperature (superheated)
    t = t_suc1            ! define state
    
    call TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
    rho_suc1 = D*wm             ! assign density to rho_suc1 [kg/m^3] [in g/L] (mol/L * g/mol = g/L)
    s_suc1 = s/wm*1000          ! entropy from (J/mol.K)  to  [J/(kg.K)]
    u_suc1 = e/wm*1000          ! internal energy [J/kg]
    h_suc1 = h/wm*1000          ! enthalpy (J/kg)
    cp_suc1 = cp/wm*1000        ! specific heat in J/(kg.K)
    d = D
    call TRNPRP (t,d,x,eta,tcx,ierr,herr)       ! Getting Viscosity and thermal conductivity
    miu_suc1 = eta*1e-6  ! Dynamic Viscosity
    k_suc1 = tcx    ! Thermal conductivity
    
    
    write (31,1002), 'Refrigerant          =  ', fluid
    write (31,1000), 'Saturation Pressure  =  ', p_suc1, ' kPa'
    write (31,1000), 'Suction temperature  =  ', t_suc1 - 273.15, ' Celsius'
    write (31,1000), 'Density              =  ', rho_suc1, ' kg/m^3 or g/L'
    write (31,1000), 'Entropy              =  ', s_suc1,  ' J/(kg.K)'
    write (31,1000), 'Specific Heat        =  ', cp_suc1, ' J/(kg.K)'
    write (31,1001), 'Enthalpy             =  ', h_suc1,  ' J/kg'
    write (31,1001), 'Internal Energy      =  ', u_suc1,  ' J/kg'
    write (31,1001), 'Dynamic Viscosity    =  ', miu_suc1, ' Pa.s (Pa.s) or N.s/m^2'
    write (31,1000), 'Thermal Conductivity =  ', k_suc1, ' W/(m.K)'
    write (31,1000), 'Molecular Weight     =  ', wm, ' g/mol'
    
    print *, ' -------------------- Thermodynamic Properties @ suction -------------------- '
    write (6,1000), 'Saturation Pressure  =  ', p_suc1, ' kPa'
    write (6,1000), 'Suction temperature  =  ', t_suc1 - 273.15, ' Celsius'
    write (6,1000), 'Density              =  ', rho_suc1, ' kg/m^3 or g/L'
    write (6,1000), 'Entropy              =  ', s_suc1,  ' J/(kg.K)'
    write (6,1000), 'Specific Heat        =  ', cp_suc1, ' J/(kg.K)'
    write (6,1001), 'Enthalpy             =  ', h_suc1,  ' J/kg'
    write (6,1001), 'Internal Energy      =  ', u_suc1,  ' J/kg'
    write (6,1001), 'Dynamic Viscosity    =  ', miu_suc1, ' Pa.s (Pa.s) or N.s/m^2'
    write (6,1000), 'Thermal Conductivity =  ', k_suc1, ' W/(m.K)'
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' '


    
  
1000 format (1x,a25,f15.4,a)
1001 format (1x,a25,es15.4,a)
1002 format (1x,a25,a25)  
end subroutine working_fluid_double_suction
    
    
    
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