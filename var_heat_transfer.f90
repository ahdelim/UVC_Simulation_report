! --- Wetted area, perimeter and hydaulic diameter
    double precision, dimension (1:no_data+1) :: A_scoc, A_scoco, A_scro, A_dcoc, A_dcoco, A_dcro, A_wv, A_wsc, A_wdc, P_wsc, P_wdc, D_wsc, D_wdc
    ! --- Hydraulic diameter (constant)
    double precision D_hc
    ! --- Characteristic velocity
    double precision Vel_scro, Vel_dcro, Vel_scoc, Vel_dcoc, Vel_ocohc
    ! --- Reynolds number, Prandt number, Nusselt Number, Heat transfer coefficient (h)
    double precision Re_scoc, Pr_scoc, Nu_scoc, Re_ocohc, Pr_ocohc, Nu_ocohc, Re_scro, Pr_scro, Nu_scro, Re_dcro, Pr_dcro, Nu_dcro, Re_dcoc, Pr_dcoc, Nu_dcoc
    double precision, dimension (1:no_data+1) :: ht_scoc, ht_ocohc, ht_scro, ht_dcro, ht_dcoc, ht_scv, ht_dcv 
    double precision R_schc, R_scro, R_dcsc, R_dchc, R_dcro, R_scoco, R_dcoco
    ! --- Heat transfer rate
    double precision Q_schc, Q_scro, Q_dcsc, Q_dchc, Q_dcro
    ! --- Temperatures, density, viscosity, specific heat, thermal conductivity
    double precision T_hc, T_room
    double precision, dimension (1:no_data+1) :: T_sc, T_roi, T_dc, T_scoco, T_dcoco
    double precision rho_scoc, miu_scoc, cp_scoc, k_scoc
    double precision rho_ocohc, miu_ocohc, cp_ocohc, k_ocohc
    double precision rho_scro, miu_scro, cp_scro, k_scro
    double precision rho_dcro, miu_dcro, cp_dcro, k_dcro
    double precision rho_dcoc, miu_dcoc, cp_dcoc, k_dcoc
    
    ! -----------------------------------------------
    ! Heat transfer variables for vertical direction
    ! -----------------------------------------------
    
    ! --- Wetted area
    double precision, dimension (1:no_data+1) :: A_schc_vd, A_rohcsc_vd, A_rohcdc_vd, A_dchc_vd, A_scoil_vd, A_rooilsc_vd, A_rooildc_vd, A_dcoil_vd, A_rohc_vd, A_rooil_vd
    ! --- Hydraulic diameter, Characteristic Length, clearance, thickness (constant)
    double precision D_upoil, D_lowoil, L_c
    ! --- Constant properties
    double precision T_oil, VelG_oil_top, VelG_oil_bottom, viscous_oil, T_resoil, Pr_resoil    ! temporary set them as constant
    ! --- Natural Convective HT coefficient in Oil Reservoir
    double precision Gr_resoil, Ra_resoil, Nu_resoil
    double precision, dimension (1:no_data+1) :: T_oco, ht_N        ! T_oco is the outer cylinder bottom temperature
    ! --- Viscous Dissipation of Oil Flow in Clearance 
    double precision T_wm, Br_oil_top, Br_oil_bottom, Nu_oil1, Nu_oil2, Nu_oil1_bottom, Nu_oil2_bottom
    double precision, dimension (1:no_data+1) :: ht_upoil1, ht_upoil2, ht_lowoil1, ht_lowoil2
    ! --- In-chamber heat transfer coefficient in vertical direction
    double precision, dimension (1:no_data+1) :: ht_schc_vd, ht_dchc_vd, ht_scoil_vd, ht_dcoil_vd
    ! --- Heat transfer resistance in vertical direction (Top and Bottom)
    double precision R_schc_vd, R1_rohc_vd, R2_rohc_vd, R_dchc_vd !R1_rohcsc_vd, R2_rohcsc_vd, R1_rohcdc_vd, R2_rohcdc_vd, 
    double precision R_scoil_vd, R1_rooil_vd, R2_rooil_vd, R_dcoil_vd !R1_rooilsc_vd, R2_rooilsc_vd, R1_rooildc_vd, R2_rooildc_vd  
    ! --- Heat transfer rate in vertical direction (Top and Bottom)
    double precision Q_schc_vd, Q1_rohc_vd, Q2_rohc_vd, Q_dchc_vd !Q1_rohcsc_vd, Q2_rohcsc_vd, Q1_rohcdc_vd, Q2_rohcdc_vd, Q_dchc_vd
    double precision Q_scoil_vd, Q1_rooil_vd, Q2_rooil_vd, Q_dcoil_vd !Q1_rooilsc_vd, Q2_rooilsc_vd, Q1_rooildc_vd, Q2_rooildc_vd, Q_dcoil_vd
    ! --- Lower and Upper boundaries for Rayleigh number
    double precision, parameter :: LowBound = 1.d5, UpperBound = 1.d10
    ! --- Iteration Parameters
    !double precision T_ite, T_ite2, j, Q_dcro_ite, Q_scro_ite, T_wm_ite, Br_oil_ite, Nu_oil1_ite, Nu_oil2_ite, ht_upoil1_ite, ht_upoil2_ite, ht_lowoil1_ite, ht_lowoil2_ite
    !double precision R1_rohc_vd_ite, R1_rooil_vd_ite, Q1_rohc_vd_ite, Q1_rooil_vd_ite, Error_T_roi !R1_rohcsc_vd_ite, R1_rohcdc_vd_ite, R1_rooilsc_vd_ite, R1_rooildc_vd_ite, Q1_rohcsc_vd_ite, Q1_rohcdc_vd_ite, Q1_rooilsc_vd_ite,  Q1_rooildc_vd_ite, 
    ! --- Dummy variables for heat transfer correlation (Liu and Zhou)
    double precision, parameter :: C = 0.75, m = 0.8, n = 0.6
    ! --- Iteration Convergent Criterion
    !double precision, parameter :: heat_conv_criterion = 0.01!, ht_data = 5000
    !double precision, dimension (1:no_data+1) :: Q_roller, deltaT_roller
    ! ----- Global parameter and variables for exergy analysis ------ !
    double precision T_roller_piston, T_vanehousing
    common/heat_temperature/T_hc, T_oil, T_resoil, T_roller_piston, T_vanehousing
    double precision, dimension (1:no_data+1) :: Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex
    ! ------------ Output --------------------- !
    ! --- Total Heat Transfer in various chambers
    double precision, dimension (1:no_data+1) :: dqdtheta_sc,  dqdtheta_dc, dqdtheta_hc, dqdtheta_resoil, dqdtheta_oil
    
    
    
    
    ! include "var_heat_transfer.f90"