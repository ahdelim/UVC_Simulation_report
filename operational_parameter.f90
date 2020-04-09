subroutine operational_parameter
    
    
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    include "var_clearance.f90"
    common/model_opt/journal_opt, heat_opt, leakage_opt, oil_network_opt, optimization_opt
    
    ! Physical constant
    read(112,*) pi      
    read(112,*) g_grav  ! [m/s2] Gravitational acceleration
    ! Universal Gas Constant
    !read(112,*) R_universal     ! [J/mol.K] Universal Gas Constant
    read(112,*) T_0     ! [C] environment/atmosphere temperature
    T_0 = T_0 + 273.15  ! convert [C] to [K]
    read(112,*) p_0     ! [kPa] environment/atm pressure
    ! material properties
    read(112,*) rho_mat ! [kg/m3] Density of material   (steel = 7800)
    read(112,*) k_mat ![W/(m.K)] Thermal conductivity (steel = 42.6)
    
    ! Efficiency
    read(112,*) eff_is  ! [ - ] Isentropic Efficiency
    
    ! --------------------
    ! Coefficient of friction
    ! --------------------
    read(112,*) coef_vs ! [-] Coefficient of friction vane side
    read(112,*) coef_vhs    ! [-] Coefficient of friction at vane housing side (or vane tip)
    read(112,*) coef_circ_lip   ! [N/m] Tangential friction per unit circumference (from Figure 9.12, K.M. Tan)
    ! --------------------
    ! Lubricant properties
    ! --------------------
    read(112,*) miu_oil ! [Ns/m2][Pa.s] !standard and supplier data ! 1cSt = 10e-6 m2/s (unit)
    read(112,*) rho_oil ! [kg/m3]       ! oil density
    read(112,*) beta_oil	![1/C]	! Thermal Expansion (olive oil)
    read(112,*) cp_oil	! [J/kg.K]	! Specific heat capacity @100C
    read(112,*) k_oil	!   [W/m.K]		! Thermal Conductivity @100C
    
    read(112,*) T_oil_sump  ! [C]
    read(112,*) T_oil_flow  ! [C]
    !pi = 3.14159265359
    !g_grav = 9.81    ![m/s2]
    
    ! Operating conditions
    print *, ' Please input the compressor speed in RPM (i.e. 3000)'
    read (*,*), rev
    !read(112,*) rev     ! [rpm] Revolution per minute  ! read from input file
    !read(112,*) q_load  ! [W] Cooling load
    !q_load = 5275.28            ! [W] for 1000 m2 space
    freq = rev/60.0           ! frequency
    omega_1 = 2*pi*freq         ! operating speed [rad/s]
    alpha_1 = 0.0
    read(112,*) rev_b     ! [RPM] angular speed of bearing
    omega_b = 2*pi*rev_b/60.0       ! [rad/s] angular speed of bearing
    ! Ports discharge coefficient
    read(112,*) coef_sucport ![-]     Suction port of compressor
    read(112,*) coef_discport ![-]    Discharge port of compressor
    read(112,*) coef_vef    ! [-]   vane endface leakage flow coefficient
    read(112,*) coef_ref    ! [-]   rotor endface leakage flow coefficient
    read(112,*) coef_rad    ! [-]   radial clearance leakage flow coefficient
    !coef_sucport_1 = 0.61
    !coef_discport_1 = 0.61
    
    ! Discharge valve parameters
    read(112,*) rho_dv  ![kg/m3] density
    read(112,*) E_dv    ![GPa]
    E_dv = E_dv*1.0d9   ![N/m2 or Pa] Young's Modulus
    read(112,*) dpratio_dv  !Damping Ratio of DV    dpratio_dv = c/(2*rho_dv*area_dv*natural_freq)
    read(112,*) coef_dv     ! coefficient to evaluate the effective force area
    read(112,*) coef_dv_back ! coefficient to evaluate the effective force area for rectangular valve
    read(112,*) sur_tension_oil ! [N/m] interfacial surface tension of oil/lubricant
    read(112,*) angle_oil_film  ! [degree] contact angle of the meniscus between valve and oil film
    angle_oil_film = angle_oil_film*pi/180.0        ! convert to rad
    !rho_dv = 7800.0   ![kg/m3] density
    !E_dv = 200.0*1.d9  ![N/m2 or Pa] Young's Modulus
    !dpratio_dv = 0.2     !Damping Ratio of DV
    
    ! clearances
    read(112,*) cl_upend    ! upper endface clearance [micron-m]
    read(112,*) cl_lowend   ! lower endface clearance [micron-m]
    read(112,*) cl_vs       ! vane side clearance   [micron-m]
    read(112,*) cl_rad_vh   ! vane housing radial clearance [micron-m]
    read(112,*) cl_rad_ro   ! rotor radial clearance between outer cylinder [micron-m]
    
    read(112,*) cl_bear_s_up    ! upper shaft bearing radial clearance [micron-m]
    read(112,*) cl_bear_s   ! shaft bearing radial clearance [micron-m]
    read(112,*) cl_rad_roll ! roller radial clearance between rotor [micron-m]
    
    read(112,*) cl_eccef_up		! cl_effef_up	! upper endface clearance of eccentric (mm)
    read(112,*) cl_eccef_low		! cl_effef_low	! lower endface clearance of eccentric (mm)
    
    ! ----------------------------
    ! Convert clearance from [micron] to [m] (SI Unit)
    ! ----------------------------
    cl_upend = cl_upend*1.d-6   !   [m]
    cl_lowend = cl_lowend*1.d-6     ![m]
    cl_vs = cl_vs*1.d-6         ! [m]
    cl_rad_vh = cl_rad_vh*1.d-6     ! [m]
    cl_rad_ro = cl_rad_ro*1.d-6     ! [m]
    cl_bear_s_up = cl_bear_s_up*1.d-6   ! [m]
    cl_bear_s = cl_bear_s*1.d-6     !   [m]
    cl_rad_roll = cl_rad_roll*1.d-6     ! [m]
    cl_eccef_up = cl_eccef_up*1.d-3     ! [mm] --> [m]
    cl_eccef_low = cl_eccef_low*1.d-3   ! [m]

    
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Physical Constants "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) ' Pi                                 pi                  = ', pi, ' -' 
    write (113,2989) " Gravitational Acceleration         g_grav              = ", g_grav, " m/s2"
    write (113,*) ' '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Material Properties "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) " Density of material                rho_mat             = ", rho_mat, " kg/m3"
    write (113,2989) " Thermal Conductivity of material   k_mat               = ", k_mat, " W/m.K"
    write (113,*) ' '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Operational Parameters "
    write (113,*) " -------------------------------------------------------------------- "
    !write (113,2989) " Cooling Load                       q_load              = ", q_load, " W"
    write (113,2989) " Operating Speed [rpm]              rev                 = ", rev, " rev/min"
    write (113,2989) " Operating Speed                    omega_1             = ", omega_1, " rad/s"
    write (113,2989) " Frequency                          freq                = ", freq, " Hz"
    write (113,2989) " Coefficient of Discharge                               = ", coef_discport, " -"
    write (113,2989) " Isentropic coefficient for flow    eff_is              = ", eff_is, " -"
    write (113,2989) " Vane side frictional coefficient   coef_vs             = ", coef_vs, " -"
    write (113,2989) " Vane Housing side (tip) fric. coef coef_vhs            = ", coef_vhs, " -"
    write (113,2989) " Tangential friction per unit circ. coef_circ_lip       = ", coef_circ_lip, " N/m"
    write (113,2989) " Dynamic Viscosity of oil lubricant miu_oil             = ", miu_oil, " Ns/m2"
    write (113,2989) " Oil lubricant reservoir Temp.      T_oil_sump          = ", T_oil_sump, " C"
    write (113,2989) " Oil lubricant in-chamber Temp.     T_oil_flow          = ", T_oil_flow, " C"
    write (113,*) ' '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Valve Reed Physical Properties "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) " Density of Valve Reed         rho_dv         = ", rho_dv, " kg/m3"
    write (113,2989) " Young Modulus's of Valve Reed E_dv           = ", E_dv/10.0**9, " GPa"
    write (113,2989) " Damping Ratio of Valve Reed   dpratio_dv     = ", dpratio_dv, " -"
    write (113,2989) " Force coefficient             coef_dv        = ", coef_dv, " -"
    write (113,*) ' '
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Clearances "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) " Clearance of upper endface          cl_upend             = ", cl_upend*1.0d6, " micron-m"
    write (113,2989) " Clearance of lower endface          cl_lowend            = ", cl_lowend*1.0d6, " micron-m"
    write (113,2989) " Clearance of vane side              cl_vs                = ", cl_vs*1.0d6, " micron-m"
    write (113,2989) " Clearance of vane housing (radial)  cl_rad_vh            = ", cl_rad_vh*1.0d6, " micron-m"
    write (113,2989) " Clearance of rotor (radial)         cl_rad_ro            = ", cl_rad_ro*1.0d6, " micron-m"
    write (113,2989) " Clearance of shaft upper bearing (radial) cl_bear_s_up   = ", cl_bear_s_up*1.0d6, " micron-m"
    write (113,2989) " Clearance of shaft lower bearing (radial) cl_bear_s      = ", cl_bear_s*1.0d6, " micron-m"
    write (113,2989) " Clearance of roller (radial)        cl_rad_roll          = ", cl_rad_roll*1.0d6, " micron-m"
    write (113,2989) " Clearance of eccentric upper endface cl_eccef_up         = ", cl_eccef_up*1.0d6, " micron-m"
    write (113,2989) " Clearance of eccentric upper endface cl_eccef_low        = ", cl_eccef_low*1.0d6, " micron-m"
    write (113,*) ' '
    

2989 format (1x,A,F10.4,A)
 
end subroutine operational_parameter
    
    