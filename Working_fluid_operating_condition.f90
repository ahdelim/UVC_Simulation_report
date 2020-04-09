subroutine working_fluid_operating_condition(fluid)
    
    ! REFPROP variables
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1, ncmax = 20
    dimension z(ncmax),x(ncmax),y(ncmax), xliq(ncmax), xvap(ncmax)
    character(255) :: fluid
    
    include "var_operating_fluid_condition.f90"
    common/fluid_info/wm            ! wm = molecular weight in [g/mol]
  
    read (112,*) temp_suct_dew     ! 7.2 AHRI Standard
    read (112,*) temp_disc_dew     ! 54.4 AHRI Standard
    read (112,*) t_superheated      ! 27.8 (AHRI Standard) superheated temperature 
    read (112,*) p_suc_air         ! Air suction pressure (bar)
    read (112,*) t_suc_air         ! Air suction temperature (C)
    read (112,*) p_disc_air         ! Air discharge pressure (bar)
    read (112,*) t_disc_air         ! Air discharge temperature (C)
    t_low = temp_suct_dew + 273.15  ! Evaporator Temperature in Kelvin [K]
    t_high = temp_disc_dew + 273.15 ! Condenser Temperature in Kelvin [K]
    !t_low = 7.2 + 273.15  ! Evaporator Temperature in Kelvin [K]
    !t_high = 54.4 + 273.15 ! Condenser Temperature in Kelvin [K]

    
    
    ! -------------------------------------------------------------------------------------------------------------------------- !
    !  Suction Condition  
    ! -------------------------------------------------------------------------------------------------------------------------- !
    i = 2
    t = t_low
    call SATT(t,x,i,p,dl,dv,xliq,xvap,ierr,herr)
    
    p_suc = p              ! suction pressure of 1st stage compressor = saturated pressure obtained from refprop [kPa]
    !t_suc1 = 35.0 + 273.15  ! After heat exchanger (Evaporator), superheated state [K]
    t_suc = t_low + t_superheated  ! Return Gas Temperature (superheated state) [K], As stated by AHRI Standard 540
    t = t_suc
    ! -------------------------------
    ! Account for Air
    ! -------------------------------
    if (fluid .eq. 'AIR.PPF') then
        p_suc = p_suc_air*100.0     ! Atmospheric pressure [kPa]
        p = p_suc
        t_suc = t_suc_air + 273.15
        t = t_suc
    endif
    
    call TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
    
    rho_suc = D*wm             ! assign density to rho_suc1 [kg/m^3] or [g/L] (mol/L *  g/mol = g/L), wm is in g/mol
    s_suc = s/wm*1000.0          ! entropy [J/(kg.K)] (J/mol.K / g/mol = J/g.K, J/g.K * 1000 = J/kg.K)
    u_suc = e/wm*1000.          ! internal energy [J/kg]
    h_suc = h/wm*1000.0          ! enthalpy [J/kg]
    cv_suc = cv/wm*1000.0       ! specific heat with constant volume in [J/(kg.K)]
    cp_suc = cp/wm*1000.0        ! specific heat in [J/(kg.K)]
    
    !d = D
    !call TRNPRP (t,d,x,eta,tcx,ierr,herr)       ! Getting Viscosity and thermal conductivity
    
    rho = D
    
    call TRNECS (t,rho,x,eta,tcx,ierr,herr)
    if (ierr.ne.0) then
        write (6,*) 'Error in getting thermodynamics properties...'
        write (6,*) ierr,herr
    endif
    miu_suc = eta*1.d-6  ! Dynamic Viscosity [Pa.s or N.s/m^2]
    k_suc = tcx    ! Thermal conductivity [W/m.K]
    
    ! ---------------------------------------------------------------------------------------------------------------------------- !
    !  Discharge Condition 
    ! ---------------------------------------------------------------------------------------------------------------------------- !
    
    s = s_suc/1000.0*wm  ! Isentropic compression process (assumption) [J/mol.K]
    
    if (fluid .eq. 'CO2.FLD') then
        p_disc = 100.0*(10*10) ! disc. pressure in kPa
        p = p_disc
        t_disc = 46.1 + 273.15 ! disc. temperature in Kelvin
    elseif (fluid .eq. 'AIR.PPF') then
        p_disc = p_disc_air*(10*10) ! disc. pressure in kPa
        p = p_disc
        t_disc = t_disc_air + 273.15  ! disc temperature in [K]
    else
        t = t_high      ! define state (condenser)
        i = 2
        
        call SATT(t,x,i,p,dl,dv,xliq,xvap,ierr,herr)
        p_disc = p      ! disc. pressure = Saturated pressure at condenser temp. [kPa]
    end if
    write(101,*) s
    call PSFLSH (p_disc,s,x,tt,dd,dl,dv,xliq,xvap,q,e,h1,cv,cp,w,ierr,herr)  ! calculate discharge temperature from p and s
    
    if (fluid .eq. 'AIR.PPF') then
        t_disc = t_disc_air + 273.15  ! disc temperature in [K]
    else
        t_disc = tt         ! Discharge temperature (superheated state) [K]
    endif
    
    call TPFLSH (tt,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)       ! Getting Thermodynamic properties (superheated)
    
    rho_disc = D*wm ! assign density to rho_disc [kg/m^3] [g/L]
    s_disc = s/wm*1000.0          ! entropy [J/(kg.K)]
    u_disc = e/wm*1000.0          ! internal energy [J/kg]
    h_disc = h/wm*1000.0          ! enthalpy [J/kg]
    cv_disc = cv/wm*1000.0       ! specific heat with constant volume in [J/(kg.K)]
    cp_disc = cp/wm*1000.0        ! specific heat in [J/(kg.K)]
    
    !d = D
    !call TRNPRP (tt,d,x,eta,tcx,ierr,herr)       ! Getting Viscosity and thermal conductivity
    rho = D
    if (fluid .eq. 'AIR.PPF') then
        call TRNECS (t_disc,rho,x,eta,tcx,ierr,herr)
    else
        call TRNECS (t,rho,x,eta,tcx,ierr,herr)
    endif
    
    if (ierr.ne.0) then
        write (6,*) 'Error in getting thermodynamics properties...'
        write (6,*) ierr, herr
    endif
    miu_disc = eta*1.d-6  ! Dynamic Viscosity [Pa.s or N.s/m2]
    k_disc = tcx    ! Thermal conductivity   [W/(m.K)]
    
    
    
    
    !write (31,*), ' -------------------- Thermodynamic Properties @ suction -------------------- '
    !write (31,1000), 'Saturation Pressure  =  ', p_suc1, ' kPa'
    !write (31,1000), 'Suction temperature  =  ', t_suc1 - 273.15, ' Celsius'
    !write (31,1000), 'Density              =  ', rho_suc1, ' kg/m^3 or g/L'
    !write (31,1000), 'Entropy              =  ', s_suc1,  ' J/(kg.K)'
    !write (31,1000), 'Specific Heat        =  ', cp_suc1, ' J/(kg.K)'
    !write (31,1001), 'Enthalpy             =  ', h_suc1,  ' J/kg'
    !write (31,1001), 'Internal Energy      =  ', u_suc1,  ' J/kg'
    !write (31,1001), 'Dynamic Viscosity    =  ', miu_suc1, ' Pa.s (Pa.s) or N.s/m^2'
    !write (31,1000), 'Thermal Conductivity =  ', k_suc1, ' W/(m.K)'
    !write (31,*), ' '
    !write (31,*), ' -------------------- Thermodynamic Properties @ Discharge ------------------ '
    !write (31,1000), 'Saturation Pressure  =  ', p_disc, ' kPa'
    !write (31,1000), 'Discharge Temp.      =  ', t_disc - 273.15, ' Celsius'
    !write (31,1000), 'Density              =  ', rho_disc, ' kg/m^3 or g/L'
    !write (31,1000), 'Entropy              =  ', s_disc,  ' J/(kg.K)'
    !write (31,1000), 'Specific Heat        =  ', cp_disc, ' J/(kg.K)'
    !write (31,1001), 'Enthalpy             =  ', h_disc,  ' J/kg'
    !write (31,1001), 'Internal Energy      =  ', u_disc,  ' J/kg'
    !write (31,1001), 'Dynamic Viscosity    =  ', miu_disc, ' Pa.s (Pa.s) or N.s/m^2'
    !write (31,1000), 'Thermal Conductivity =  ', k_disc, ' W/(m.K)'
    !write (31,*) ' '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*), ' Thermodynamics Properties @ suction '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,1000), 'Saturation Pressure  =  ', p_suc, ' kPa'
    write (113,1000), 'Evaporator Temp.     =  ', t_low - 273.15, ' Celsius'
    write (113,1000), 'Suction temperature  =  ', t_suc - 273.15, ' Celsius'
    write (113,1000), 'Density              =  ', rho_suc, ' kg/m^3 or g/L'
    write (113,1000), 'Entropy              =  ', s_suc,  ' J/(kg.K)'
    write (113,1000), 'Specific Heat (Cv)   =  ', cv_suc, ' J/(kg.K)'
    write (113,1000), 'Specific Heat        =  ', cp_suc, ' J/(kg.K)'
    write (113,1001), 'Enthalpy             =  ', h_suc,  ' J/kg'
    write (113,1001), 'Internal Energy      =  ', u_suc,  ' J/kg'
    write (113,1001), 'Dynamic Viscosity    =  ', miu_suc, ' Pa.s (Pa.s) or N.s/m^2'
    write (113,1000), 'Thermal Conductivity =  ', k_suc, ' W/(m.K)'
    write (113,*), ' '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*), ' Thermodynamics Properties @ Discharge '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,1000), 'Saturation Pressure  =  ', p_disc, ' kPa'
    write (113,1000), 'Condenser Temp.      =  ', t_high - 273.15, ' Celsius'
    write (113,1000), 'Discharge Temp.      =  ', t_disc - 273.15, ' Celsius'
    write (113,1000), 'Density              =  ', rho_disc, ' kg/m^3 or g/L'
    write (113,1000), 'Entropy              =  ', s_disc,  ' J/(kg.K)'
    write (113,1000), 'Specific Heat (Cv)   =  ', cv_disc, ' J/(kg.K)'
    write (113,1000), 'Specific Heat        =  ', cp_disc, ' J/(kg.K)'
    write (113,1001), 'Enthalpy             =  ', h_disc,  ' J/kg'
    write (113,1001), 'Internal Energy      =  ', u_disc,  ' J/kg'
    write (113,1001), 'Dynamic Viscosity    =  ', miu_disc, ' Pa.s (Pa.s) or N.s/m^2'
    write (113,1000), 'Thermal Conductivity =  ', k_disc, ' W/(m.K)'
    write (113,*) ' '
    
    print *, ' -------------------- Thermodynamics Properties @ suction -------------------- '
    write (6,1000), 'Saturation Pressure  =  ', p_suc, ' kPa'
    write (6,1000), 'Suction Dew Temp.    =  ', t_low - 273.15, ' Celsius'
    write (6,1000), 'Suction temperature  =  ', t_suc - 273.15, ' Celsius'
    write (6,1000), 'Density              =  ', rho_suc, ' kg/m^3 or g/L'
    write (6,1000), 'Entropy              =  ', s_suc,  ' J/(kg.K)'
    write (6,1000), 'Specific Heat (Cv)   =  ', cv_suc, ' J/(kg.K)'
    write (6,1000), 'Specific Heat        =  ', cp_suc, ' J/(kg.K)'
    write (6,1001), 'Enthalpy             =  ', h_suc,  ' J/kg'
    write (6,1001), 'Internal Energy      =  ', u_suc,  ' J/kg'
    write (6,1001), 'Dynamic Viscosity    =  ', miu_suc, ' Pa.s (Pa.s) or N.s/m^2'
    write (6,1000), 'Thermal Conductivity =  ', k_suc, ' W/(m.K)'
    print *, ' ----------------------------------------------------------------------------- '
    print *, ' '
    print *, ' -------------------- Thermodynamics Properties @ discharge ------------------ '
    write (6,1000), 'Saturation Pressure  =  ', p_disc, ' kPa'
    write (6,1000), 'Discharge Dew Temp.  =  ', t_high - 273.15, ' Celsius'
    write (6,1000), 'Discharge Temp.      =  ', t_disc - 273.15, ' Celsius'
    write (6,1000), 'Density              =  ', rho_disc, ' kg/m^3 or g/L'
    write (6,1000), 'Entropy              =  ', s_disc,  ' J/(kg.K)'
    write (6,1000), 'Specific Heat (Cv)   =  ', cv_disc, ' J/(kg.K)'
    write (6,1000), 'Specific Heat        =  ', cp_disc, ' J/(kg.K)'
    write (6,1001), 'Enthalpy             =  ', h_disc,  ' J/kg'
    write (6,1001), 'Internal Energy      =  ', u_disc,  ' J/kg'
    write (6,1001), 'Dynamic Viscosity    =  ', miu_disc, ' Pa.s (Pa.s) or N.s/m^2'
    write (6,1000), 'Thermal Conductivity =  ', k_disc, ' W/(m.K)'
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' '
  
1000 format (1x,a25,f15.4,a)
1001 format (1x,a25,es15.4,a)
1002 format (1x,a25,a25)  
end subroutine working_fluid_operating_condition
    
    
    
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