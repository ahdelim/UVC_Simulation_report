subroutine working_fluid_double_discharge
    
    ! REFPROP variables
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: fluid, herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1
    
    include "var_operating_fluid_condition.f90"
    common/fluid_info/wm
    ! t_low = 7.2 + 273.15  ! Suction temperature in Kelvin (from Evaporator)
    t_high = 54.4 + 273.15 ! Discharge temperature in Kelvin (to Condenser)

    !call INFO (1,wm,ttp,tnbp,tc,pc,dc,zc,acf,dip,rgas)
    s_disc = s_suc1/1000*wm  ! Isentropic compression process

    if (fluid .eq. 'CO2.FLD') then
        p_disc = 100*(10*10) ! disc. pressure in kPa
        t_disc = 46.1 + 273.15 ! disc. temperature in Kelvin
    else
        t = t_high      ! define state (condenser)
        i = 2
        call SATT(t,x,i,p,dl,dv,xliq,xvap,ierr,herr)
        p_disc = p      ! disc. pressure in kPa
    end if
    
    ! ------------------------------- Discharge Condition ----------------------------------- !
    s = s_disc          ! isentropic assumption
    p = p_disc          ! same pressure in condenser line
    call PSFLSH (p,s,x,tt,dd,dl,dv,xliq,xvap,q,e,h1,cv,cp,w,ierr,herr)  ! calculate discharge temperature from p and s
    t_disc = tt         ! Discharge temperature (superheated)
    t = t_disc          
    p = p_disc
    call TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)       ! Getting Thermodynamic properties (superheated)
    rho_disc = D*wm ! assign density to rho_disc (in kg/m^3) (in g/L)
    s_disc = s/wm*1000          ! entropy J/(kg.K)
    u_disc = e/wm*1000          ! internal energy (J/kg)
    h_disc = h/wm*1000          ! enthalpy (J/kg)
    cp_disc = cp/wm*1000        ! specific heat in J/(kg.K)
    d = D
    call TRNPRP (t,d,x,eta,tcx,ierr,herr)       ! Getting Viscosity and thermal conductivity
    miu_disc = eta*1e-6  ! Dynamic Viscosity [Pa.s / N.s/m2]
    k_disc = tcx    ! Thermal conductivity   [W/(m.K)]
    
    
    write (32,1000), 'Saturation Pressure  =  ', p_disc, ' kPa'
    write (32,1000), 'Discharge Temp.      =  ', t_disc - 273.15, ' Celsius'
    write (32,1000), 'Density              =  ', rho_disc, ' kg/m^3 or g/L'
    write (32,1000), 'Entropy              =  ', s_disc,  ' J/(kg.K)'
    write (32,1000), 'Specific Heat        =  ', cp_disc, ' J/(kg.K)'
    write (32,1001), 'Enthalpy             =  ', h_disc,  ' J/kg'
    write (32,1001), 'Internal Energy      =  ', u_disc,  ' J/kg'
    write (32,1001), 'Dynamic Viscosity    =  ', miu_disc, ' Pa.s (Pa.s) or N.s/m^2'
    write (32,1000), 'Thermal Conductivity =  ', k_disc, ' W/(m.K)'
    
    print *, ' -------------------- Thermodynamic Properties @ discharge ------------------ '
    write (6,1000), 'Saturation Pressure  =  ', p_disc, ' kPa'
    write (6,1000), 'Discharge Temp.      =  ', t_disc - 273.15, ' Celsius'
    write (6,1000), 'Density              =  ', rho_disc, ' kg/m^3 or g/L'
    write (6,1000), 'Entropy              =  ', s_disc,  ' J/(kg.K)'
    write (6,1000), 'Specific Heat        =  ', cp_disc, ' J/(kg.K)'
    write (6,1001), 'Enthalpy             =  ', h_disc,  ' J/kg'
    write (6,1001), 'Internal Energy      =  ', u_disc,  ' J/kg'
    write (6,1001), 'Dynamic Viscosity    =  ', miu_disc, ' Pa.s (Pa.s) or N.s/m^2'
    write (6,1000), 'Thermal Conductivity =  ', k_disc, ' W/(m.K)'
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' '
    

    
    
1000 format (1x,a25,f15.4,a)
1001 format (1x,a25,es15.4,a)    
end subroutine working_fluid_double_discharge
    
    
    
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