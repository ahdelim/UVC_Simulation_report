subroutine refprop_ini(fluid)
    
    ! REFPROP variables
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(3) :: hrf
    Integer, parameter :: nc = 1, ncmax = 20
    character(255) :: herr, hfmix
    character(255), dimension (1:ncmax) :: fluid
    Integer input_fluid, check_int
    
    include "var_physical_constants.f90"
    common/fluid_info/wm        ! common molecular weight, specific gas constant
    
    ! -------------------------------- Select Working Fluid ----------------------------------- !
301 continue    
    print *, ' ---> Please select working fluid (Input number 1 - 5)' ! Possible fluids - R22.fld, R134a.fld, R1234YF.fld, R32, CO2
    print *, ' '
    print *, ' 1 - R22'
    print *, ' 2 - R134a'
    print *, ' 3 - R1234yf'
    print *, ' 4 - R32'
    print *, ' 5 - CO2'
    print *, ' 6 - R410a'
    print *, ' 7 - Air'
    print *, ' 8 - R290 (Propane)'
    print *, ' '
    read (*,*,iostat=check_int), input_fluid
    print *, ' '
    
    !input_fluid = 3        ! default the fluid if not user input
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*), ' Working Fluid Properties  '
    write (113,*) " -------------------------------------------------------------------- "
    
    hrf = 'DEF'
    hfmix = 'hmx.bnc'
    if (check_int == 0) then
        if (input_fluid .eq. 1) then
            fluid(1) = 'R22.FLD'
            print *, 'Selected working fluid = R22'
            !write (31,*), ' Refrigerant          =       R22'
            write (113,*), ' Refrigerant          =         R22'
        elseif (input_fluid .eq. 2) then
            fluid(1) = 'R134A.FLD'
            print *, 'Selected working fluid = R134a'
            !write (31,*), ' Refrigerant          =       R134a'
            write (113,*), ' Refrigerant          =         R134a'
        elseif (input_fluid .eq. 3) then
            fluid(1) = 'R1234YF.FLD'
            print *, 'Selected working fluid = R1234yf'
            !write (31,*), ' Refrigerant          =       R1234yf'
            write (113,*), ' Refrigerant          =         R1234yf'
        elseif (input_fluid .eq. 4) then
            fluid(1) = 'R32.FLD'
            print *, 'Selected working fluid = R32' 
            !write (31,*), ' Refrigerant          =       R32'
            write (113,*), ' Refrigerant          =         R32'
        elseif (input_fluid .eq. 5) then
            fluid(1) = 'CO2.FLD'
            print *, 'Selected working fluid = CO2'
            !write (31,*), ' Refrigerant          =       CO2'
            write (113,*), ' Refrigerant          =         CO2'
        elseif (input_fluid .eq. 6) then
            fluid(1) = 'R410A.PPF'
            print *, 'Selected working fluid = R410a'
            !write (31,*), ' Refrigerant          =       R410a'
            write (113,*), ' Refrigerant          =         R410a'
        elseif (input_fluid .eq. 7) then
            fluid(1) = 'AIR.PPF'
            print *, 'Selected working fluid = Air'
            !write (31,*), ' Refrigerant          =       Air'
            write (113,*), ' Refrigerant          =         Air'
        elseif (input_fluid .eq. 8) then
            fluid(1) = 'PROPANE.FLD'
            print *, 'Selected working fluid = Air'
            !write (31,*), ' Refrigerant          =       Air'
            write (113,*), ' Refrigerant          =         R290 (PROPANE)'
        end if
    else
        Print*, "Input integer number only! Please re-enter!"
        print *, ' '
        goto 301
    endif
    print *, ' '
    
    ! ---------------------------------------------------------------------------------------------- !
    ! -------------------------------- Initialization of REFPROP ----------------------------------- !
    
    call SETPATH('C:\Program Files (x86)\REFPROP')
    call SETUP(nc, fluid, hfmix, hrf, ierr, herr)
    call INFO (1,wm,ttp,tnbp,tc,pc,dc,zc,acf,dip,rgas)
    if (ierr.ne.0) then
        write (6,*) 'Error in REFPROP initialization...'
        write (6,*) herr
    endif
    R_universal = 8.3144598 ! [J/mol.K]	! Universal Gas Constant
    R_specific = R_universal/wm*1.d3        ! Specific gas constant [J/kg.K]
    !write (31,1000), 'Molecular Weight     =  ', wm, ' g/mol'       ! write into operating condition file
    !write (31,*), ' '
    write (113,1000), 'Molecular Weight         = ', wm, ' g/mol'       ! write into overview file
    write (113,1000), 'Specific Gas Constant    = ', R_specific, ' J/kg.K'  
    write (113,*), ' '
    
1000 format (1x,a25,f15.4,a)
1001 format (1x,a25,es15.4,a)
1002 format (1x,a25,a25)
     
endsubroutine refprop_ini

    
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

