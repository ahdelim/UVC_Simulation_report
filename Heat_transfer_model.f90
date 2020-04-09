subroutine heat_transfer_m(theta_1, theta_2, v_com, v_suc, dlvdt, dgammadt_1, l_v, p_scv, m_scv, t_scv, u_scv, rho_scv, h_scv, s_scv, miu_scv, cv_scv, cp_scv, k_scv, p_dcv, m_dcv, t_dcv, u_dcv, rho_dcv, h_dcv, s_dcv, miu_dcv, cv_dcv, cp_dcv, k_dcv, dqdtheta_sc, dqdtheta_dc, dqdtheta_hc, dqdtheta_resoil, dqdtheta_oil, T_ocsc, T_ocdc, T_rosc, T_rodc, T_roller, T_uocsc, T_locsc, T_uocdc, T_locdc, T_ocsc2, T_ocsc3, T_ocdc2, T_ocdc3, T_rosc2, T_rosc3, T_rodc2, T_rodc3, T_roller2, T_roller3, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex)
    implicit none
    include "var_simulation_parameter.f90"
    include "var_Main_dimensions.f90"
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    include "var_operating_fluid_condition.f90"
    include "var_geometrical.f90"
    include "var_thermo.f90"
    include "var_compressor_evaluation.f90"
    include "var_clearance.f90"
    
    ! ---------------------------------------------------------------------- !
    ! Heat Transfer Model of NVC analysed by W.X. and Y.D.
    ! This file is created on 29-Jan-2018
    ! subscripts meaning are listed below
    ! ----------------------------------------------
    ! Rearranged by Y.D. Lim on 02-April-2018
    ! Arrays are taken out to save memory
    ! ----------------------------------------------
    ! ----------------------------------------------------------------------
    
    ! ----- Simulation ----------- !
    integer i, j, k
    ! ------ Input ------ !
    double precision, dimension (1:no_data+1) :: dlvdt, dgammadt_1
    ! ------ Dummy variables ------- !
    double precision vel_avrg, T_lub 
    ! --------- Heat Transfer variables ------------ !
    include "var_heat_transfer.f90"
    
    ! ---- Variables For Gauss Elimination Method ------ !
    include "var_heat_transfer_gauss.f90"
    
    
    
    ! -----------------------------------
    ! Constant Parameters definition
    ! -----------------------------------
    vel_avrg = (r_roi)*1.e-3*omega_1  !0.5*(e + r_roi + r_shaft)*1.d-3*omega_1   ! Average velocity [m/s]
    T_lub = 2.*pi*(r_ecc*1.d-3)*(l_ecc*1.d-3)*miu_oil*vel_avrg/cl_rad_roll*r_ecc*1.d-3        ! [Nm] or [J] Area*Stress*moment_arm, assume constans moment arm, torque due to lubricant dragging friction
    L_c = (0.5*r_oco)*1.d-3         ! [m] characteristic length
    D_hc = ((4.*(r_hc-r_oco)*l_com)/(2.*(r_hc-r_oc+l_com)))*1.d-3       ! [m] Hydraulic diameter of housing chamber
    D_upoil = 2.*cl_upend     ! [m] Hydraulic Diameter of upper oil flow
    D_lowoil = 2.*cl_lowend   ! [m] Hydraulic Diameter of lower oil flow
    VelG_oil_top = 0.1      ! [m/s] Assumed average oil velocity in the top endface clearance (Based on 3000 RPM)
    VelG_oil_bottom = 0.03  ! [m/s] Assumed average oil velocity in the bottom endface clearance (Based on 3000 RPM)
    T_resoil = (T_oil_sump + 273.15)      ! [K] Oil reservoir (Oil sump) Temperature (assumed constant)
    T_oil = (T_oil_flow + 273.15)        ! [K] Oil Temperature in the clearances (assumed constant)
    T_hc = t_disc               ! [K] Housing chamber temperature
    T_room = T_0 + 273.15            ! [K] room temperature
    viscous_oil = miu_oil/rho_oil       ! [m2/s] Kinematic Viscosity of Oil lubricant
    Pr_resoil = cp_oil*miu_oil/k_oil    ! Prantdl number
    ! --------------------------------------
    ! Assign Oil properties
    ! --------------------------------------
    rho_ocohc = rho_disc ! 85.66496
    miu_ocohc = miu_disc ! 1.378379*1.d-5
    cp_ocohc = cp_disc   ! 1.260567*1.d-3
    k_ocohc = k_disc     ! 1.802938*1.d-2
    
    !===============================================
    ! --- Constant Parameters for Gauss Elimination
    !===============================================
    r_ococo = (r_oc+r_oco)/2.
    l_net = l_com !-t_oc_cover-t_oc_base      ! height of chamber
    A_ocscdc = (l_net/3.0)*r_ococo*1.d-6        ! average area of the compressor wall, heat flux, divided by 3 (FEM)
    r_roiro = (r_roi+r_ro)/2.
    A_roscdc = (l_net/3.0)*r_roiro*1.d-6
    A_cover_cross = r_oc*t_oc_cover*1.d-6 ! r_oco*t_oc_cover*1.d-6
    A_base_cross = r_oc*t_oc_base*1.d-6 ! r_oco*t_oc_base*1.d-6
    ! ===============================================
    ! This loop is basically for finding Heat Transfer Coefficient ht_xxxx
    ! Area, hydraulic diameter and temperature are assigned accordingly
    ! -----------------------------------------------
        
    do 800 i = 1, no_data+1
        A_scoc(i) = theta_1(i)*l_com*r_oc*1.d-6
        A_scoco(i) = theta_1(i)*l_com*r_oco*1.d-6
        A_scro(i) = theta_2(i)*l_com*r_ro*1.d-6
        A_dcoc(i) = (2.*pi-theta_1(i))*l_com*r_oc*1.d-6
        A_dcoco(i) = (2.*pi-theta_1(i))*l_com*r_oco*1.d-6
        A_dcro(i) = (2.*pi-theta_2(i))*l_com*r_ro*1.d-6
        A_wv(i) = l_v(i)*l_com*1.d-6
        A_wsc(i) = A_wv(i)+A_scro(i)
        A_wdc(i) = A_wv(i)+A_dcro(i)
        P_wsc(i) = 2.*(theta_2(i)*r_ro+l_v(i)+l_com)*1.d-3
        P_wdc(i) = 2.*((2*pi-theta_2(i))*r_ro+l_v(i)+l_com)*1.d-3
        
        if (P_wsc(i) == 0.0) then
            D_wsc(i) = 0.0
        else
            D_wsc(i) = 4.*A_wsc(i)/P_wsc(i)
        end if
        
        if (P_wdc(i) == 0.0) then
            D_wdc(i) = 0.0
        else
            D_wdc(i) = 4.*A_wdc(i)/P_wdc(i)
        end if        
        
       
        ! --- For vertical direction
        A_schc_vd(i) = (v_suc(i)/l_com)*1.d-6
        !A_rohcsc_vd(i) = 0.5*(r_ro**2)*theta_2(i)*1.d-6
        !A_rohcdc_vd(i) = pi*(r_ro**2)*1.d-6 - A_rohcsc_vd(i)
        A_rohc_vd(i) = pi*(r_ro**2)*1.d-6
        A_dchc_vd(i) = (v_com(i)/l_com)*1.d-6! - A_schc_vd(i)
        A_scoil_vd(i) = A_schc_vd(i) 
        !A_rooilsc_vd(i) = A_rohcsc_vd(i)
        !A_rooildc_vd(i) = A_rohcdc_vd(i)
        A_rooil_vd(i) = A_rohc_vd(i)
        A_dcoil_vd(i) = A_dchc_vd(i)
        
        ! --- assign input to local array
        T_sc(i) = t_scv(i)
        T_dc(i) = t_dcv(i)
        
        rho_scoc = rho_scv(i)
        miu_scoc = miu_scv(i)
        cp_scoc = cp_scv(i)
        k_scoc = k_scv(i)
        
        rho_dcoc = rho_dcv(i)
        miu_dcoc = miu_dcv(i)
        cp_dcoc = cp_dcv(i)
        k_dcoc = k_dcv(i)

        Vel_scro = 0.5*sqrt((dlvdt(i)*1.d-3)**2 + (r_ro*1.d-3*dgammadt_1(i))**2) !0.5*(abs(dlvdt(i)*1.d-3) + abs(r_ro*1.d-3*dgammadt_1(i)))   ! [m/s] velocity near to the rotor in suction chamber
        Vel_dcro = Vel_scro          ! [m/s] velocity near to the rotor in discharge chamber (of working fluid)
        Vel_scoc = Vel_scro          ! [m/s] velocity near to the OC in suction chamber (of working fluid)
        Vel_dcoc = Vel_scro          ! [m/s] velocity near to the OC in discharge chamber    (of working fluid)
        Vel_ocohc = mass_total / (rho_ocohc*2.0*((r_hc*1.d-3)-(r_oco*1.d-3))*(l_com*1.d-3))       ! [m/s] Mass flow divided by (rho*Area) = velocity
        
        ! --- For ht_scoc = ht_scro ---+ HT in Suction near OC
        if (D_wsc(i) == 0.0) then  !.or. theta_2(i) < pi/4
            ht_scoc(i) = 0.0
            ht_scro(i) = 0.0
            ht_scv(i) = 0.0
        else
            Re_scoc = rho_scoc*Vel_scoc*D_wsc(i)/miu_scoc
            Pr_scoc = cp_scoc*miu_scoc/k_scoc
            Nu_scoc = C*(Re_scoc)**m*(Pr_scoc)**n
            ht_scoc(i) = Nu_scoc*k_scoc/D_wsc(i)
            ht_scro(i) = ht_scoc(i)
            ht_scv(i) = ht_scro(i)
        end if
            
        ! --- For ht_ocohc ---+ HT in Housing Chamber
        Re_ocohc = rho_ocohc*Vel_ocohc*D_hc/miu_ocohc
        Pr_ocohc = cp_ocohc*miu_ocohc/k_ocohc
        if (Pr_ocohc >=0.5 .and. Pr_ocohc <=1.5 .and. Re_ocohc >= 1.d4 .and. Re_ocohc <= 5.d6) then
            Nu_ocohc = 0.0214*(Re_ocohc**0.8-100.)*Pr_ocohc**0.4 !!Gnielinski Correlation
        else if (Pr_ocohc >=1.5 .and. Pr_ocohc <=500.0 .and. Re_ocohc >= 3.d3 .and. Re_ocohc <= 1.d6) then
            Nu_ocohc = 0.012*(Re_ocohc**0.87-280.)*Pr_ocohc**0.4 !!Gnielinski Correlation
        end if
        ht_ocohc(i) = Nu_ocohc*k_ocohc/D_hc
            
        ! --- For ht_dcoc = ht_dcro ---+ HT in Discharge near OC
        if (D_wdc(i) == 0.0) then ! .or. theta_2(i) > 3.*pi/4.
            ht_dcoc(i) = 0.0
            ht_dcro(i) = 0.0
            ht_dcv(i) = 0.0
        else
            Re_dcoc = rho_dcoc*Vel_dcoc*D_wdc(i)/miu_dcoc
            Pr_dcoc = cp_dcoc*miu_dcoc/k_dcoc
            Nu_dcoc = C*Re_dcoc**m*Pr_dcoc**n
            ht_dcoc(i) = Nu_dcoc*k_dcoc/D_wdc(i)
            ht_dcro(i) = ht_dcoc(i)
            ht_dcv(i) = ht_dcro(i)
        end if
            
        ! --- For in-chamber heat transfer in vertical direction
        ht_schc_vd(i) = ht_scoc(i)
        ht_scoil_vd(i) = ht_schc_vd(i)
        ht_dchc_vd(i) = ht_dcoc(i)
        ht_dcoil_vd(i) = ht_dchc_vd(i)
        
        ! --- Natural Convective HT Coefficient
        T_oco(i) = 0.5*(T_sc(i) + T_dc(i)) !assume the OC temp is in btween T_sc and T_dc, outer cylinder outer surface
        Gr_resoil = (g_grav*beta_oil*abs(T_oco(i)-T_resoil)*L_c**3)/((viscous_oil)**2)
        Ra_resoil = Gr_resoil*Pr_resoil
        
        ! --- if the condition is always satisfied, then the if-else statement can be removed
        if (Ra_resoil >= LowBound .and. Ra_resoil <= UpperBound) then
            Nu_resoil = 0.27*Ra_resoil**0.25
            ht_N(i) = Nu_resoil*k_oil/L_c
        end if
        
800 continue
    
    ! ----------------------------------------------
    ! Finite Element Analysis
    ! ------------------------    
    ! Compressor is splited into 19 elements
    ! Values of each elements are assigned and later
    ! solved by Gauss Elimination
    ! Results : Temperature variation of each elements
    !===================================
do 810 i = 1, no_data+1 !ht_data+1
        
        l_ocsc = r_ococo*theta_1(i)
        l_ocdc = r_ococo*(2.0*pi-theta_1(i))
        l_rosc = r_roiro*theta_2(i)
        l_rodc = r_roiro*(2.0*pi-theta_2(i))
        A_ocsc_cross = 0.5*theta_1(i)*(r_oco**2-r_oc**2)*1.d-6
        A_ocdc_cross = 0.5*(2.0*pi-theta_1(i))*(r_oco**2-r_oc**2)*1.d-6
        A_rosc_cross = 0.5*theta_2(i)*(r_ro**2-r_roi**2)*1.d-6
        A_rodc_cross = 0.5*(2.0*pi-theta_2(i))*(r_ro**2-r_roi**2)*1.d-6
        A_scroller_cross = 0.5*theta_2(i)*(r_roi**2)*1.d-6
        A_dcroller_cross = 0.5*(2.0*pi-theta_2(i))*(r_roi**2)*1.d-6
        l_uocsc = 0.5*r_oco*theta_1(i)
        l_uocdc = 0.5*r_oco*(2.0*pi-theta_1(i))
        l_locsc = l_uocsc
        l_locdc = l_uocdc
        
        !=========================
        ! --- Element 1 -- ocsc
        !=========================
        C_ocsc = 0.0
        H_ocsc_sc = ht_scoc(i)*A_scoc(i)/3.0     ! [W/K]
        H_ocsc_hc = ht_ocohc(i)*A_scoco(i)/3.0
        H_ocsc_ocdc = 2.0*( (k_mat*A_ocscdc) / (0.5*(l_ocsc + l_ocdc)*1.d-3) )
        H_ocsc_uocsc = (k_mat*A_ocsc_cross) / (0.5*((l_net/3.0) + t_oc_cover)*1.d-3)
        H_ocsc_ocsc2 = (k_mat*A_ocsc_cross) / ((l_net/3.0)*1.d-3)
        
        U_ocsc_ocsc = H_ocsc_sc + H_ocsc_hc + H_ocsc_ocdc + H_ocsc_uocsc + H_ocsc_ocsc2
        U_ocsc_sc = - H_ocsc_sc
        U_ocsc_hc = - H_ocsc_hc
        U_ocsc_ocdc = - H_ocsc_ocdc
        U_ocsc_uocsc = - H_ocsc_uocsc
        U_ocsc_ocsc2 = - H_ocsc_ocsc2
        
        !=========================
        ! --- Element 1.2 -- ocsc2  (Element 2)
        !=========================
        C_ocsc = 0.0
        H_ocsc2_sc = ht_scoc(i)*A_scoc(i)/3.0
        H_ocsc2_hc = ht_ocohc(i)*A_scoco(i)/3.0
        H_ocsc2_ocdc2 = 2.0*( (k_mat*A_ocscdc) / (0.5*(l_ocsc + l_ocdc)*1.d-3) )
        H_ocsc2_ocsc = (k_mat*A_ocsc_cross) / ((l_net/3.0)*1.d-3)
        H_ocsc2_ocsc3 = (k_mat*A_ocsc_cross) / ((l_net/3.0)*1.d-3)
        
        U_ocsc2_ocsc2 = H_ocsc2_sc + H_ocsc2_hc + H_ocsc2_ocdc2 + H_ocsc2_ocsc + H_ocsc2_ocsc3
        U_ocsc2_sc = - H_ocsc2_sc
        U_ocsc2_hc = - H_ocsc2_hc
        U_ocsc2_ocdc2 = - H_ocsc2_ocdc2
        U_ocsc2_ocsc = - H_ocsc2_ocsc
        U_ocsc2_ocsc3 = - H_ocsc2_ocsc3
        
        !=========================
        ! --- Element 1.3 -- ocsc3 (Element 3)
        !=========================
        C_ocsc3 = 0.0
        H_ocsc3_sc = ht_scoc(i)*A_scoc(i)/3.0
        H_ocsc3_hc = ht_ocohc(i)*A_scoco(i)/3.0
        H_ocsc3_ocdc3 = 2.0*( (k_mat*A_ocscdc) / (0.5*(l_ocsc + l_ocdc)*1.d-3) )
        H_ocsc3_ocsc2 = (k_mat*A_ocsc_cross) / ((l_net/3.0)*1.d-3)
        H_ocsc3_locsc = (k_mat*A_ocsc_cross) / (0.5*((l_net/3.0) + t_oc_base)*1.d-3)
       
        U_ocsc3_ocsc3 = H_ocsc3_sc + H_ocsc3_hc + H_ocsc3_ocdc3 + H_ocsc3_ocsc2 + H_ocsc3_locsc
        U_ocsc3_sc = - H_ocsc3_sc
        U_ocsc3_hc = - H_ocsc3_hc
        U_ocsc3_ocdc3 = - H_ocsc3_ocdc3
        U_ocsc3_ocsc2 = - H_ocsc3_ocsc2
        U_ocsc3_locsc = - H_ocsc3_locsc
 
        !=========================
        ! --- Element 2 -- ocdc (Element 4)
        !=========================
        C_ocdc = 0.0
        H_ocdc_dc = ht_dcoc(i)*A_dcoc(i)/3.0
        H_ocdc_hc = ht_ocohc(i)*A_dcoco(i)/3.0
        H_ocdc_ocsc = 2.0*( (k_mat*A_ocscdc) / (0.5*(l_ocsc + l_ocdc)*1.d-3) )
        H_ocdc_uocdc = (k_mat*A_ocdc_cross) / (0.5*((l_net/3.0) + t_oc_cover)*1.d-3)
        H_ocdc_ocdc2 = (k_mat*A_ocdc_cross) / ((l_net/3.0)*1.d-3)
        
        U_ocdc_ocdc = H_ocdc_dc + H_ocdc_hc + H_ocdc_ocsc + H_ocdc_uocdc + H_ocdc_ocdc2
        U_ocdc_dc = - H_ocdc_dc
        U_ocdc_hc = - H_ocdc_hc
        U_ocdc_ocsc = - H_ocdc_ocsc
        U_ocdc_uocdc = - H_ocdc_uocdc
        U_ocdc_ocdc2 = - H_ocdc_ocdc2
        
        !=========================
        ! --- Element 2.2 -- ocdc2 (Element 5)
        !=========================
        C_ocdc2 = 0.0
        H_ocdc2_dc = ht_dcoc(i)*A_dcoc(i)/3.0
        H_ocdc2_hc = ht_ocohc(i)*A_dcoco(i)/3.0
        H_ocdc2_ocsc2 = 2.0*( (k_mat*A_ocscdc) / (0.5*(l_ocsc + l_ocdc)*1.d-3) )
        H_ocdc2_ocdc = (k_mat*A_ocdc_cross) / ((l_net/3.0)*1.d-3)
        H_ocdc2_ocdc3 = (k_mat*A_ocdc_cross) / ((l_net/3.0)*1.d-3)
        
        U_ocdc2_ocdc2 = H_ocdc2_dc + H_ocdc2_hc + H_ocdc2_ocsc2 + H_ocdc2_ocdc + H_ocdc2_ocdc3
        U_ocdc2_dc = - H_ocdc2_dc
        U_ocdc2_hc = - H_ocdc2_hc
        U_ocdc2_ocsc2 = - H_ocdc2_ocsc2
        U_ocdc2_ocdc = - H_ocdc2_ocdc
        U_ocdc2_ocdc3 = - H_ocdc2_ocdc3
        
        !=========================
        ! --- Element 2.3 -- ocdc3 (Element 6)
        !=========================
        C_ocdc3 = 0.0
        H_ocdc3_dc = ht_dcoc(i)*A_dcoc(i)/3.0
        H_ocdc3_hc = ht_ocohc(i)*A_dcoco(i)/3.0
        H_ocdc3_ocsc3 = 2.0*( (k_mat*A_ocscdc) / (0.5*(l_ocsc + l_ocdc)*1.d-3) )
        H_ocdc3_ocdc2 = (k_mat*A_ocdc_cross) / ((l_net/3.0)*1.d-3)
        H_ocdc3_locdc = (k_mat*A_ocdc_cross) / (0.5*((l_net/3.0) + t_oc_base)*1.d-3)
        
        U_ocdc3_ocdc3 = H_ocdc3_dc + H_ocdc3_hc + H_ocdc3_ocsc3 + H_ocdc3_ocdc2 + H_ocdc3_locdc
        U_ocdc3_dc = - H_ocdc3_dc
        U_ocdc3_hc = - H_ocdc3_hc
        U_ocdc3_ocsc3 = - H_ocdc3_ocsc3
        U_ocdc3_ocdc2 = - H_ocdc3_ocdc2
        U_ocdc3_locdc = - H_ocdc3_locdc
    
        !=========================
        ! --- Element 3 -- rosc (Element 7)
        !=========================
        C_rosc = (theta_2(i) / (2.0*pi))*0.5*T_lub*omega_1/3.0  ! [W] assume friction heat is generated
        H_rosc_sc = ht_scro(i)*A_scro(i)/3.0
        H_rosc_roller = (theta_2(i)*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_rosc_rodc = 2.0*( (k_mat*A_roscdc) / (0.5*(l_rosc + l_rodc)*1.d-3) )
        H_rosc_uocsc = (k_mat*A_rosc_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_rosc_rosc2 = (k_mat*A_rosc_cross) / ((l_net/3.0)*1.d-3)
        
        U_rosc_rosc = H_rosc_sc + H_rosc_roller + H_rosc_rodc + H_rosc_uocsc + H_rosc_rosc2
        U_rosc_sc = - H_rosc_sc
        U_rosc_roller = - H_rosc_roller
        U_rosc_rodc = - H_rosc_rodc
        U_rosc_uocsc = - H_rosc_uocsc
        U_rosc_rosc2 = - H_rosc_rosc2
        
        !=========================
        ! --- Element 3.2 -- rosc2 (Element 8)
        !=========================
        C_rosc2 = (theta_2(i) / (2.0*pi))*0.5*T_lub*omega_1/3.0
        H_rosc2_sc = ht_scro(i)*A_scro(i)/3.0
        H_rosc2_roller2 = (theta_2(i)*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_rosc2_rodc2 = 2.0*( (k_mat*A_roscdc) / (0.5*(l_rosc + l_rodc)*1.d-3) )
        H_rosc2_rosc = (k_mat*A_rosc_cross) / ((l_net/3.0)*1.d-3)
        H_rosc2_rosc3 = (k_mat*A_rosc_cross) / ((l_net/3.0)*1.d-3)
        
        U_rosc2_rosc2 = H_rosc2_sc + H_rosc2_roller2 + H_rosc2_rodc2 + H_rosc2_rosc + H_rosc2_rosc3
        U_rosc2_sc = - H_rosc2_sc
        U_rosc2_roller2 = - H_rosc2_roller2
        U_rosc2_rodc2 = - H_rosc2_rodc2
        U_rosc2_rosc = - H_rosc2_rosc
        U_rosc2_rosc3 = - H_rosc2_rosc3

        !=========================
        ! --- Element 3.3 -- rosc3 (Element 9)
        !=========================
        C_rosc3 = (theta_2(i) / (2.0*pi))*0.5*T_lub*omega_1/3.0
        H_rosc3_sc = ht_scro(i)*A_scro(i)/3.0
        H_rosc3_roller3 = (theta_2(i)*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_rosc3_rodc3 = 2.0*( (k_mat*A_roscdc) / (0.5*(l_rosc + l_rodc)*1.d-3) )
        H_rosc3_rosc2 = (k_mat*A_rosc_cross) / ((l_net/3.0)*1.d-3)
        H_rosc3_locsc = (k_mat*A_rosc_cross) / (0.5*((l_net/3.0) + t_oc_base)*1.d-3)
        
        U_rosc3_rosc3 = H_rosc3_sc + H_rosc3_roller3 + H_rosc3_rodc3 + H_rosc3_rosc2 + H_rosc3_locsc
        U_rosc3_sc = - H_rosc3_sc
        U_rosc3_roller3 = - H_rosc3_roller3
        U_rosc3_rodc3 = - H_rosc3_rodc3
        U_rosc3_rosc2 = - H_rosc3_rosc2
        U_rosc3_locsc = - H_rosc3_locsc
        
        !=========================
        ! --- Element 4 -- rodc (Element 10)
        !=========================
        C_rodc = ((2.0*pi-theta_2(i)) / (2*pi))*0.5*T_lub*omega_1/3.0 ! [W] assume friction heat is generated
        H_rodc_dc = ht_dcro(i)*A_dcro(i)/3.0
        H_rodc_roller = ((2.0*pi-theta_2(i))*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_rodc_rosc = 2.0*( (k_mat*A_roscdc) / (0.5*(l_rosc + l_rodc)*1.d-3) )
        H_rodc_uocdc = (k_mat*A_rodc_cross) / (0.5*((l_net/3.0) + t_oc_cover)*1.d-3)
        H_rodc_rodc2 = (k_mat*A_rodc_cross) / ((l_net/3.0)*1.d-3)
        
        U_rodc_rodc = H_rodc_dc + H_rodc_roller + H_rodc_rosc + H_rodc_uocdc + H_rodc_rodc2
        U_rodc_dc = - H_rodc_dc
        U_rodc_roller = - H_rodc_roller
        U_rodc_rosc = - H_rodc_rosc
        U_rodc_uocdc = - H_rodc_uocdc
        U_rodc_rodc2 = - H_rodc_rodc2
        
        !=========================
        ! --- Element 4.2 -- rodc2 (Element 11)
        !=========================
        C_rodc2 = ((2.0*pi-theta_2(i)) / (2*pi))*0.5*T_lub*omega_1/3.0
        H_rodc2_dc = ht_dcro(i)*A_dcro(i)/3.0
        H_rodc2_roller2 = ((2.0*pi-theta_2(i))*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_rodc2_rosc2 = 2.0*( (k_mat*A_roscdc) / (0.5*(l_rosc + l_rodc)*1.d-3) )
        H_rodc2_rodc = (k_mat*A_rodc_cross) / ((l_net/3.0)*1.d-3)
        H_rodc2_rodc3 = (k_mat*A_rodc_cross) /((l_net/3.0)*1.d-3)
        
        U_rodc2_rodc2 = H_rodc2_dc + H_rodc2_roller2 + H_rodc2_rosc2 + H_rodc2_rodc + H_rodc2_rodc3
        U_rodc2_dc = - H_rodc2_dc
        U_rodc2_roller2 = - H_rodc2_roller2
        U_rodc2_rosc2 = - H_rodc2_rosc2
        U_rodc2_rodc = - H_rodc2_rodc
        U_rodc2_rodc3 = - H_rodc2_rodc3
    
        !=========================
        ! --- Element 4.3 -- rodc3 (Element 12)
        !=========================
        C_rodc3 = ((2.0*pi-theta_2(i)) / (2*pi))*0.5*T_lub*omega_1/3.0
        H_rodc3_dc = ht_dcro(i)*A_dcro(i)/3.0
        H_rodc3_roller3 = ((2.0*pi-theta_2(i))*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_rodc3_rosc3 = 2.0*( (k_mat*A_roscdc) / (0.5*(l_rosc + l_rodc)*1.d-3) )
        H_rodc3_rodc2 = (k_mat*A_rodc_cross) / ((l_net/3.0)*1.d-3)
        H_rodc3_locdc = (k_mat*A_rodc_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        
        U_rodc3_rodc3 = H_rodc3_dc + H_rodc3_roller3 + H_rodc3_rosc3 + H_rodc3_rodc2 + H_rodc3_locdc
        U_rodc3_dc = - H_rodc3_dc
        U_rodc3_roller3 = - H_rodc3_roller3
        U_rodc3_rosc3 = - H_rodc3_rosc3
        U_rodc3_rodc2 = - H_rodc3_rodc2
        U_rodc3_locdc = - H_rodc3_locdc

        
        !=========================
        ! --- Element 5 -- roller (Element 13)
        !=========================
        C_roller = 0.5*T_lub*omega_1/3.0 ! [W]  assume friction heat is generated
        H_roller_rosc = (theta_2(i)*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_roller_rodc = ((2.0*pi-theta_2(i))*k_mat*l_com*1.d-3) / ((3.0*(log(r_ro/r_roi))))
        H_roller_uocsc = (k_mat*A_scroller_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_roller_roller2 = (k_mat*pi*(r_roi*1.d-3)**2) / ((l_net/3.0)*1.d-3)
        H_roller_uocdc = (k_mat*A_dcroller_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        
        U_roller_roller = H_roller_rosc + H_roller_rodc + H_roller_uocsc + H_roller_roller2 + H_roller_uocdc
        U_roller_rosc = - H_roller_rosc
        U_roller_rodc = - H_roller_rodc
        U_roller_uocsc = - H_roller_uocsc
        U_roller_roller2 = - H_roller_roller2
        U_roller_uocdc = - H_roller_uocdc
        
        !========================= 
        ! --- Element 5.2 -- roller2 (Element 14)
        !=========================
        C_roller2 = 0.5*T_lub*omega_1/3.0
        H_roller2_rosc2 = (theta_2(i)*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_roller2_rodc2 = ((2.0*pi-theta_2(i))*k_mat*l_com*1.d-3) / ((3.0*(log(r_ro/r_roi))))
        H_roller2_roller = (k_mat*pi*(r_roi*1.d-3)**2) / ((l_net/3.0)*1.d-3)
        H_roller2_roller3 = (k_mat*pi*(r_roi*1.d-3)**2) / ((l_net/3.0)*1.d-3)
        
        U_roller2_roller2 = H_roller2_rosc2 + H_roller2_rodc2 + H_roller2_roller + H_roller2_roller3
        U_roller2_rosc2 = - H_roller2_rosc2
        U_roller2_rodc2 = - H_roller2_rodc2
        U_roller2_roller = - H_roller2_roller
        U_roller2_roller3 = - H_roller2_roller3

        !=========================
        ! --- Element 5.3 -- roller3 (Element 15)
        !=========================
        C_roller3 = 0.5*T_lub*omega_1/3.0
        H_roller3_rosc3 = (theta_2(i)*k_mat*l_com*1.d-3) / (3.0*(log(r_ro/r_roi)))
        H_roller3_rodc3 = ((2.0*pi-theta_2(i))*k_mat*l_com*1.d-3) / ((3.0*(log(r_ro/r_roi))))
        H_roller3_roller2 = (k_mat*pi*(r_roi*1.d-3)**2) / ((l_net/3.0)*1.d-3)
        H_roller3_locsc = (k_mat*A_scroller_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        H_roller3_locdc = (k_mat*A_dcroller_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        
        U_roller3_roller3 = H_roller3_rosc3 + H_roller3_rodc3 + H_roller3_roller2 + H_roller3_locsc + H_roller3_locdc
        U_roller3_rosc3 = - H_roller3_rosc3
        U_roller3_rodc3 = - H_roller3_rodc3
        U_roller3_roller2 = - H_roller3_roller2
        U_roller3_locsc = - H_roller3_locsc
        U_roller3_locdc = - H_roller3_locdc

        !=========================
        ! --- Element 6 -- uocsc (Element 16)
        !=========================
        C_uocsc = 0.0
        H_uocsc_sc = ht_schc_vd(i)*A_schc_vd(i)
        H_uocsc_hc = ht_ocohc(i)*(0.5*theta_1(i)*(r_oc*1.d-3)**2)
        H_uocsc_rosc = (k_mat*A_rosc_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_uocsc_ocsc = (k_mat*A_ocsc_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_uocsc_roller = (k_mat*A_scroller_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_uocsc_uocdc = 2.0*( (k_mat*A_cover_cross) / (0.5*(l_uocsc+l_uocdc)*1.d-3) )
        
        U_uocsc_uocsc = H_uocsc_sc + H_uocsc_hc + H_uocsc_rosc + H_uocsc_ocsc + H_uocsc_roller + H_uocsc_uocdc
        U_uocsc_sc = - H_uocsc_sc
        U_uocsc_hc = - H_uocsc_hc
        U_uocsc_rosc = - H_uocsc_rosc
        U_uocsc_ocsc = - H_uocsc_ocsc
        U_uocsc_roller = - H_uocsc_roller
        U_uocsc_uocdc = - H_uocsc_uocdc
        
        !=========================
        ! --- Element 7 -- locsc (Element 17)
        !=========================
        C_locsc = 0.0
        H_locsc_sc = ht_scoil_vd(i)*A_scoil_vd(i)
        H_locsc_resoil = ht_N(i)*(0.5*theta_1(i)*(r_oco*1.d-3)**2)
        H_locsc_rosc3 = (k_mat*A_rosc_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        H_locsc_ocsc3 = (k_mat*A_ocsc_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        H_locsc_roller3 = (k_mat*A_scroller_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        H_locsc_locdc = 2.0*( (k_mat*A_base_cross) / (0.5*(l_locsc+l_locdc)*1.d-3) )
        
        U_locsc_locsc = H_locsc_sc + H_locsc_resoil + H_locsc_rosc3 + H_locsc_ocsc3 + H_locsc_roller3 + H_locsc_locdc
        U_locsc_sc = - H_locsc_sc
        U_locsc_resoil = - H_locsc_resoil
        U_locsc_rosc3 = - H_locsc_rosc3
        U_locsc_ocsc3 = - H_locsc_ocsc3
        U_locsc_roller3 = - H_locsc_roller3
        U_locsc_locdc = - H_locsc_locdc       
        
        !=========================
        ! --- Element 8 -- uocdc (Element 18)
        !=========================
        C_uocdc = 0.0
        H_uocdc_dc = ht_dchc_vd(i)*A_dchc_vd(i)
        H_uocdc_hc = ht_ocohc(i)*(0.5*(2.0*pi-theta_1(i))*(r_oc*1.d-3)**2)
        H_uocdc_rodc = (k_mat*A_rodc_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_uocdc_ocdc = (k_mat*A_ocdc_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_uocdc_roller = (k_mat*A_dcroller_cross) / (0.5*((l_net/3.0)+t_oc_cover)*1.d-3)
        H_uocdc_uocsc = 2.0*( (k_mat*A_cover_cross) / (0.5*(l_uocsc+l_uocdc)*1.d-3) )
        
        U_uocdc_uocdc = H_uocdc_dc + H_uocdc_hc + H_uocdc_rodc + H_uocdc_ocdc + H_uocdc_roller + H_uocdc_uocsc
        U_uocdc_dc = - H_uocdc_dc
        U_uocdc_hc = - H_uocdc_hc
        U_uocdc_rodc = - H_uocdc_rodc
        U_uocdc_ocdc = - H_uocdc_ocdc
        U_uocdc_roller = - H_uocdc_roller
        U_uocdc_uocsc = - H_uocdc_uocsc
        
        !=========================
        ! --- Element 9 -- locdc (Element 19)
        !=========================
        C_locdc = 0.0
        H_locdc_dc = ht_dcoil_vd(i)*A_dcoil_vd(i)
        H_locdc_resoil = ht_N(i)*(0.5*(2.0*pi-theta_1(i))*(r_oco*1.d-3)**2)
        H_locdc_rodc3 = (k_mat*A_rodc_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        H_locdc_ocdc3 = (k_mat*A_ocdc_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        H_locdc_roller3 = (k_mat*A_dcroller_cross) / (0.5*((l_net/3.0)+t_oc_base)*1.d-3)
        H_locdc_locsc = 2.0*( (k_mat*A_base_cross) / (0.5*(l_locsc + l_locdc)*1.d-3) )

        U_locdc_locdc = H_locdc_dc + H_locdc_resoil + H_locdc_rodc3 + H_locdc_ocdc3 + H_locdc_roller3 + H_locdc_locsc
        U_locdc_dc = - H_locdc_dc
        U_locdc_resoil = - H_locdc_resoil
        U_locdc_rodc3 = - H_locdc_rodc3
        U_locdc_ocdc3 = - H_locdc_ocdc3
        U_locdc_roller3 = - H_locdc_roller3
        U_locdc_locsc = - H_locdc_locsc
        
        do j = 1,G_E
            do k = 1,G_E
                U_ht_element(j,k) = 0.00000        ! create a zero matrix first
            enddo
        enddo
        !print *, U_ht_element(2,6), U_ht_element(2,7)
         ! ----------- from this line onward is for checking (by showing on screen) ----------- !
           !do j=1,G_E
           !        write(6,201) (U_ht_element(j,k),k=1,G_E), C_RHS(j)
           !end do 
        ! ----------- checking end here -------------------- !
        !pause
        
        ! --- Assign U_ht_element
        U_ht_element(1,1) = U_ocsc_ocsc
        U_ht_element(1,2) = U_ocsc_ocdc
        U_ht_element(1,6) = U_ocsc_uocsc
        U_ht_element(1,10) = U_ocsc_ocsc2
        
        U_ht_element(2,1) = U_ocdc_ocsc
        U_ht_element(2,2) = U_ocdc_ocdc
        U_ht_element(2,8) = U_ocdc_uocdc
        U_ht_element(2,12) = U_ocdc_ocdc2
        
        U_ht_element(3,3) = U_rosc_rosc
        U_ht_element(3,4) = U_rosc_rodc
        U_ht_element(3,5) = U_rosc_roller
        U_ht_element(3,6) = U_rosc_uocsc
        U_ht_element(3,14) = U_rosc_rosc2
        
        U_ht_element(4,3) = U_rodc_rosc
        U_ht_element(4,4) = U_rodc_rodc
        U_ht_element(4,5) = U_rodc_roller
        U_ht_element(4,8) = U_rodc_uocdc
        U_ht_element(4,16) = U_rodc_rodc2
        
        U_ht_element(5,3) = U_roller_rosc
        U_ht_element(5,4) = U_roller_rodc
        U_ht_element(5,5) = U_roller_roller
        U_ht_element(5,6) = U_roller_uocsc
        U_ht_element(5,8) = U_roller_uocdc
        U_ht_element(5,18) = U_roller_roller2
        
        U_ht_element(6,1) = U_uocsc_ocsc
        U_ht_element(6,3) = U_uocsc_rosc
        U_ht_element(6,5) = U_uocsc_roller
        U_ht_element(6,6) = U_uocsc_uocsc
        U_ht_element(6,8) = U_uocsc_uocdc
        
        U_ht_element(7,11) = U_locsc_ocsc3
        U_ht_element(7,15) = U_locsc_rosc3
        U_ht_element(7,19) = U_locsc_roller3
        U_ht_element(7,7) = U_locsc_locsc
        U_ht_element(7,9) = U_locsc_locdc
        
        U_ht_element(8,2) = U_uocdc_ocdc
        U_ht_element(8,4) = U_uocdc_rodc
        U_ht_element(8,5) = U_uocdc_roller
        U_ht_element(8,6) = U_uocdc_uocsc
        U_ht_element(8,8) = U_uocdc_uocdc
        
        U_ht_element(9,13) = U_locdc_ocdc3
        U_ht_element(9,17) = U_locdc_rodc3
        U_ht_element(9,19) = U_locdc_roller3
        U_ht_element(9,7) = U_locdc_locsc
        U_ht_element(9,9) = U_locdc_locdc
        
        U_ht_element(10,10) = U_ocsc2_ocsc2
        U_ht_element(10,12) = U_ocsc2_ocdc2
        U_ht_element(10,1) = U_ocsc2_ocsc
        U_ht_element(10,11) = U_ocsc2_ocsc3
        
        U_ht_element(11,11) = U_ocsc3_ocsc3
        U_ht_element(11,13) = U_ocsc3_ocdc3
        U_ht_element(11,10) = U_ocsc3_ocsc2
        U_ht_element(11,7) = U_ocsc3_locsc
        
        U_ht_element(12,12) = U_ocdc2_ocdc2
        U_ht_element(12,10) = U_ocdc2_ocsc2
        U_ht_element(12,2) = U_ocdc2_ocdc
        U_ht_element(12,13) = U_ocdc2_ocdc3
        
        U_ht_element(13,13) = U_ocdc3_ocdc3
        U_ht_element(13,11) = U_ocdc3_ocsc3
        U_ht_element(13,12) = U_ocdc3_ocdc2
        U_ht_element(13,9) = U_ocdc3_locdc
        
        U_ht_element(14,14) = U_rosc2_rosc2
        U_ht_element(14,16) = U_rosc2_rodc2
        U_ht_element(14,18) = U_rosc2_roller2
        U_ht_element(14,3) = U_rosc2_rosc
        U_ht_element(14,15) = U_rosc2_rosc3
        
        U_ht_element(15,15) = U_rosc3_rosc3
        U_ht_element(15,17) = U_rosc3_rodc3
        U_ht_element(15,19) = U_rosc3_roller3
        U_ht_element(15,7) = U_rosc3_locsc
        U_ht_element(15,14) = U_rosc3_rosc2
        
        U_ht_element(16,14) = U_rodc2_rosc2
        U_ht_element(16,16) = U_rodc2_rodc2
        U_ht_element(16,18) = U_rodc2_roller2
        U_ht_element(16,4) = U_rodc2_rodc
        U_ht_element(16,17) = U_rodc2_rodc3
        
        U_ht_element(17,15) = U_rodc3_rosc3
        U_ht_element(17,17) = U_rodc3_rodc3
        U_ht_element(17,19) = U_rodc3_roller3
        U_ht_element(17,16) = U_rodc3_rodc2
        U_ht_element(17,9) = U_rodc3_locdc
        
        U_ht_element(18,14) = U_roller2_rosc2
        U_ht_element(18,16) = U_roller2_rodc2
        U_ht_element(18,18) = U_roller2_roller2
        U_ht_element(18,5) = U_roller2_roller
        U_ht_element(18,19) = U_roller2_roller3
        
        U_ht_element(19,15) = U_roller3_rosc3
        U_ht_element(19,17) = U_roller3_rodc3
        U_ht_element(19,19) = U_roller3_roller3
        U_ht_element(19,7) = U_roller3_locsc
        U_ht_element(19,9) = U_roller3_locdc
        U_ht_element(19,18) = U_roller3_roller2
                
        ! --- Assign C_RHS(n) 
        C_RHS(1) = C_ocsc - U_ocsc_sc*T_sc(i) - U_ocsc_hc*T_hc
        C_RHS(2) = C_ocdc - U_ocdc_dc*T_dc(i) - U_ocdc_hc*T_hc
        C_RHS(3) = C_rosc - U_rosc_sc*T_sc(i)
        C_RHS(4) = C_rodc - U_rodc_dc*T_dc(i)
        C_RHS(5) = C_roller
        C_RHS(6) = C_uocsc - U_uocsc_sc*T_sc(i) - U_uocsc_hc*T_room !- U_uocsc_hc*T_hc
        C_RHS(7) = C_locsc - U_locsc_sc*T_sc(i) - U_locsc_resoil*T_resoil
        C_RHS(8) = C_uocdc - U_uocdc_dc*T_dc(i) - U_uocdc_hc*T_room !U_uocdc_hc*T_hc
        C_RHS(9) = C_locdc - U_locdc_dc*T_dc(i) - U_locdc_resoil*T_resoil
        C_RHS(10) = C_ocsc2 - U_ocsc2_sc*T_sc(i) - U_ocsc2_hc*T_hc
        C_RHS(11) = C_ocsc3 - U_ocsc3_sc*T_sc(i) - U_ocsc3_hc*T_hc
        C_RHS(12) = C_ocdc2 - U_ocdc2_dc*T_dc(i) - U_ocdc2_hc*T_hc
        C_RHS(13) = C_ocdc3 - U_ocdc3_dc*T_dc(i) - U_ocdc3_hc*T_hc
        C_RHS(14) = C_rosc2 - U_rosc2_sc*T_sc(i)
        C_RHS(15) = C_rosc3 - U_rosc3_sc*T_sc(i)
        C_RHS(16) = C_rodc2 - U_rodc2_dc*T_dc(i)
        C_RHS(17) = C_rodc3 - U_rodc3_dc*T_dc(i)
        C_RHS(18) = C_roller2
        C_RHS(19) = C_roller3
        

        ! ----------- from this line onward is for checking (by showing on screen) ----------- !
        !if (theta_1(i)*180.0/pi >= 210.0 .and. theta_1(i)*180.0/pi <= 215.0) then
        !    write(6,2982) theta_1(i)*180.0/pi
        !    do j=1,G_E
        !            print *, (U_ht_element(j,k),k=1,G_E), C_RHS(j)
        !    end do 
        !    pause
        !do k = 1,G_E
        !    C_RHS(k) = C_RHS(k)       ! create a zero matrix first
        !enddo
        ! ----------- checking end here -------------------- !
        !write(124,2983) C_RHS(1), C_RHS(2), C_RHS(3), C_RHS(4), C_RHS(5), C_RHS(6), C_RHS(7), C_RHS(8), C_RHS(9)
        
        ! ------- Use Gauss Elimination to solve for sol_T(n)
        call Gauss_elimination(U_ht_element, C_RHS, sol_T, G_E)
        
        ! ------- Assign solutions to temperature of each elements
        T_ocsc(i) = sol_T(1)
        T_ocdc(i) = sol_T(2)
        T_rosc(i) = sol_T(3)
        T_rodc(i) = sol_T(4)
        T_roller(i) = sol_T(5)
        T_uocsc(i) = sol_T(6)
        T_locsc(i) = sol_T(7)
        T_uocdc(i) = sol_T(8)
        T_locdc(i) = sol_T(9)
        T_ocsc2(i) = sol_T(10)
        T_ocsc3(i) = sol_T(11)
        T_ocdc2(i) = sol_T(12)
        T_ocdc3(i) = sol_T(13)
        T_rosc2(i) = sol_T(14)
        T_rosc3(i) = sol_T(15)
        T_rodc2(i) = sol_T(16)
        T_rodc3(i) = sol_T(17)
        T_roller2(i) = sol_T(18)
        T_roller3(i) = sol_T(19)
        
810 continue
        T_vanehousing = (sum(T_ocsc) + sum(T_ocsc2) + sum(T_ocsc3) + sum(T_ocdc) + sum(T_ocdc2) + sum(T_ocdc3))/((no_data+1)*6.0)
        T_roller_piston = ( sum(T_roller)/(no_data+1) + sum(T_roller2)/(no_data+1) + sum(T_roller3)/(no_data+1) )/3.0       ! Average temperature of roller/piston
        !T_roller_piston =  sum(T_roller)/(no_data+1)
        
        ! ---------------------------------------
        ! Heat transfer rate calculation in 2D analysis
        ! Horizontally & Vertically
        ! Temperatures are found in previous Gauss Elimination (Finite element Analysis)
        ! ---------------------------------------
do 830 i = 1, no_data+1
        
        ! --- Viscous Dissipation of Oil
        T_wm = 0.5*(T_roller_piston+T_oco(i))
        !T_wm = ( T_rosc(i) + T_rodc(i) + T_roller(i) )/3.0
        Br_oil_top = (miu_oil*VelG_oil_top**2)/(k_oil*(T_wm-T_oil))
        Br_oil_bottom = (miu_oil*VelG_oil_bottom**2)/(k_oil*(T_wm-T_oil))
        Nu_oil1 = (4.0*(1.0 - 6.0*Br_oil_top))/(1.0 - Br_oil_top*48./35.)   ! Nu for wall having higher temp
        Nu_oil2 = (4.0*(1.0 + 6.0*Br_oil_top))/(1.0 + Br_oil_top*48./35.)   ! Nu for wall having lower temp
        Nu_oil1_bottom = (4.0*(1.0 - 6.0*Br_oil_bottom))/(1.0 - Br_oil_bottom*48./35.)  ! for wall having higher temp (bottom endface)
        Nu_oil2_bottom = (4.0*(1.0 + 6.0*Br_oil_bottom))/(1.0 + Br_oil_bottom*48./35.)  ! for wall having lower temp (bottom endface)
        if (T_roller_piston > T_oco(i)) then
            ht_upoil1(i) = Nu_oil1*k_oil/D_upoil
            ht_upoil2(i) = Nu_oil2*k_oil/D_upoil
            ht_lowoil1(i) = Nu_oil1_bottom*k_oil/D_lowoil
            ht_lowoil2(i) = Nu_oil2_bottom*k_oil/D_lowoil
        else if (T_oco(i) > T_roller_piston) then
            ht_upoil1(i) = Nu_oil2*k_oil/D_upoil
            ht_upoil2(i) = Nu_oil1*k_oil/D_upoil
            ht_lowoil1(i) = Nu_oil2_bottom*k_oil/D_lowoil
            ht_lowoil2(i) = Nu_oil1_bottom*k_oil/D_lowoil
        end if
        
        ! --- Thermal Resistance Btw SC and HC
        if (A_scoc(i) == 0.0 .and. A_scoco(i) == 0.0) then
            R_schc = (log(r_oco/r_oc))/(theta_1(i)*k_mat*l_com*1.d-3)
            !R_scoco = (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        else if ((ht_scoc(i) == 0.0 .or. A_scoc(i) == 0.0) .and. A_scoco(i) .ne. 0.0) then
            R_schc = (log(r_oco/r_oc))/(theta_1(i)*k_mat*l_com*1.d-3) + 1./(ht_ocohc(i)*A_scoco(i))
            !R_scoco = (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        else if (ht_scoc(i) .ne. 0.0 .and. A_scoc(i) .ne. 0.0 .and. A_scoco(i) == 0.0) then
            R_schc = 1./(ht_scoc(i)*A_scoc(i)) + (log(r_oco/r_oc))/(theta_1(i)*k_mat*l_com*1.d-3) 
            !R_scoco = 1./(ht_scoc(i)*A_scoc(i)) + (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        else
            R_schc = 1./(ht_scoc(i)*A_scoc(i)) + (log(r_oco/r_oc))/(theta_1(i)*k_mat*l_com*1.d-3) + 1./(ht_ocohc(i)*A_scoco(i))
            !R_scoco = 1./(ht_scoc(i)*A_scoc(i)) + (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        end if
         
        ! --- Heat Transfer Rate Equation Btw SC and HC
        Q_schc = (T_sc(i) - T_hc)/(omega_1*R_schc)
         
        ! --- Thermal Resistance Btw SC and Rotor
        if (ht_scro(i) == 0.0 .or. A_scro(i) == 0.0) then
            R_scro = (log(r_ro/r_roi))/(theta_2(i)*k_mat*l_com*1.d-3)
        else
            R_scro = 1./(ht_scro(i)*A_scro(i)) + (log(r_ro/r_roi))/(theta_2(i)*k_mat*l_com*1.d-3)
        end if
        
        ! --- Heat Transfer Rate Equation Btw SC and Rotor
        Q_scro = (T_sc(i)-T_roller_piston)/(omega_1*R_scro)
         
        ! --- Thermal Resistance Btw SC and DC via Vane
        if  (A_wv(i) == 0.0) then
            R_dcsc = 0.0
            Q_dcsc = 0.0
        else if (ht_scv(i) == 0.0 .and. ht_dcv(i) .ne. 0.0 .and. A_wv(i) .ne. 0.0) then
            R_dcsc = 1/(ht_dcv(i)*A_wv(i)) + w_v_ro/(k_mat*A_wv(i))
            Q_dcsc = (T_dc(i) - T_sc(i))/(omega_1*R_dcsc)
        else if (ht_scv(i) .ne. 0.0 .and. ht_dcv(i) == 0.0 .and. A_wv(i) .ne. 0.0) then
            R_dcsc = w_v_ro/(k_mat*A_wv(i)) + 1/(ht_scv(i)*A_wv(i))
            Q_dcsc = (T_dc(i) - T_sc(i))/(omega_1*R_dcsc)
        else if (ht_scv(i) == 0.0 .and. ht_dcv(i) == 0.0 .and. A_wv(i) .ne. 0.0) then
            R_dcsc = w_v_ro/(k_mat*A_wv(i))
            Q_dcsc = (T_dc(i) - T_sc(i))/(omega_1*R_dcsc)
        else
            R_dcsc = 1/(ht_dcv(i)*A_wv(i)) + w_v_ro/(k_mat*A_wv(i)) + 1/(ht_scv(i)*A_wv(i))
            Q_dcsc = (T_dc(i) - T_sc(i))/(omega_1*R_dcsc)
        end if
         
        ! --- Thermal Resistance Btw DC and HC
        if (A_dcoc(i) == 0.0 .and. A_dcoco(i) == 0.0) then
            R_dchc = (log(r_oco/r_oc))/((2.*pi-theta_1(i))*k_mat*l_com*1.d-3)
            !R_dcoco = (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        else if ((ht_dcoc(i) == 0.0 .or. A_dcoc(i) == 0.0) .and. A_dcoco(i) .ne. 0.0) then
            R_dchc = (log(r_oco/r_oc))/((2.*pi-theta_1(i))*k_mat*l_com*1.d-3) + 1/(ht_ocohc(i)*A_dcoco(i))
            !R_dcoco = (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        else if (ht_dcoc(i) .ne. 0.0 .and. A_dcoc(i) .ne. 0.0 .and. A_dcoco(i) == 0.0) then
            R_dchc = 1/(ht_dcoc(i)*A_dcoc(i)) + (log(r_oco/r_oc))/((2.*pi-theta_1(i))*k_mat*l_com*1.d-3)
            !R_dcoco = 1/(ht_dcoc(i)*A_dcoc(i)) + (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        else
            R_dchc = 1/(ht_dcoc(i)*A_dcoc(i)) + (log(r_oco/r_oc))/((2.*pi-theta_1(i))*k_mat*l_com*1.d-3) + 1/(ht_ocohc(i)*A_dcoco(i))
            !R_dcoco = 1/(ht_dcoc(i)*A_dcoc(i)) + (log(r_oco/r_oc))/(2.*pi*k_mat*l_com*1.d-3)
        end if
         
        ! --- Heat Transfer Rate Equation Btw DC and HC
        Q_dchc = (T_dc(i) - T_hc)/(omega_1*R_dchc)
         
        ! --- Thermal Resistance Btw DC and Rotor
        if (ht_dcro(i) == 0.0 .or. A_dcro(i) == 0.0) then
            R_dcro = (log(r_ro/r_roi))/((2.*pi-theta_2(i))*k_mat*l_com*1.d-3)
        else
            R_dcro = 1/(ht_dcro(i)*A_dcro(i)) + (log(r_ro/r_roi))/((2.*pi-theta_2(i))*k_mat*l_com*1.d-3)
        end if
        
        ! --- Heat Transfer Rate Equation Btw DC and Rotor
        Q_dcro = (T_dc(i)-T_roller_piston)/(omega_1*R_dcro)
        
        ! --------------------------------------------------------
        ! For heat transfer resistance in vertical direction
        ! --------------------------------------------------------
        
        if (ht_schc_vd(i) == 0.0) then
            R_schc_vd = (t_oc_cover*1.d-3)/(k_mat*A_schc_vd(i))+1/(ht_ocohc(i)*A_schc_vd(i))
        else
            R_schc_vd = 1.0/(ht_schc_vd(i)*A_schc_vd(i))+(t_oc_cover*1.d-3)/(k_mat*A_schc_vd(i))+1.0/(ht_ocohc(i)*A_schc_vd(i))
        end if
                
        !R1_rohcsc_vd(i) = 1/(ht_upoil1(i)*A_rohcsc_vd(i))
        !R2_rohcsc_vd(i) = 1/(ht_upoil2(i)*A_rohcsc_vd(i))+(t_oc_cover*1.d-3)/(k_mat*A_rohcsc_vd(i))+1/(ht_ocohc(i)*A_rohcsc_vd(i))
        !R1_rohcdc_vd(i) = 1/(ht_upoil1(i)*A_rohcdc_vd(i))
        !R2_rohcdc_vd(i) = 1/(ht_upoil2(i)*A_rohcdc_vd(i))+(t_oc_cover*1.d-3)/(k_mat*A_rohcdc_vd(i))+1/(ht_ocohc(i)*A_rohcdc_vd(i))
        
        R1_rohc_vd = 1.0/(ht_upoil1(i)*A_rohc_vd(i))
        R2_rohc_vd = 1.0/(ht_upoil2(i)*A_rohc_vd(i))+(t_oc_cover*1.d-3)/(k_mat*A_rohc_vd(i))+1.0/(ht_ocohc(i)*A_rohc_vd(i))
        
        if (ht_dchc_vd(i) == 0.0) then
            R_dchc_vd = (t_oc_cover*1.d-3)/(k_mat*A_dchc_vd(i))+1/(ht_ocohc(i)*A_dchc_vd(i))
        else
            R_dchc_vd = 1.0/(ht_dchc_vd(i)*A_dchc_vd(i))+(t_oc_cover*1.d-3)/(k_mat*A_dchc_vd(i))+1.0/(ht_ocohc(i)*A_dchc_vd(i))
        end if
        
        if (ht_scoil_vd(i) == 0.0) then
            R_scoil_vd = (t_oc_base*1.d-3)/(k_mat*A_scoil_vd(i))+1/(ht_N(i)*A_scoil_vd(i))
        else
            R_scoil_vd = 1.0/(ht_scoil_vd(i)*A_scoil_vd(i))+(t_oc_base*1.d-3)/(k_mat*A_scoil_vd(i))+1.0/(ht_N(i)*A_scoil_vd(i))
        end if
        
        !R1_rooilsc_vd(i) = 1/(ht_lowoil1(i)*A_rooilsc_vd(i))
        !R2_rooilsc_vd(i) = 1/(ht_lowoil2(i)*A_rooilsc_vd(i))+(t_oc_base*1.d-3)/(k_mat*A_rooilsc_vd(i))+1/(ht_N(i)*A_rooilsc_vd(i))
        !R1_rooildc_vd(i) = 1/(ht_lowoil1(i)*A_rooildc_vd(i))
        !R2_rooildc_vd(i) = 1/(ht_lowoil2(i)*A_rooildc_vd(i))+(t_oc_base*1.d-3)/(k_mat*A_rooildc_vd(i))+1/(ht_N(i)*A_rooildc_vd(i))
        
        R1_rooil_vd = 1./(ht_lowoil1(i)*A_rooil_vd(i))
        R2_rooil_vd = 1./(ht_lowoil2(i)*A_rooil_vd(i))+(t_oc_base*1.d-3)/(k_mat*A_rooil_vd(i))+1/(ht_N(i)*A_rooil_vd(i))
                
        if (ht_dcoil_vd(i) == 0.0) then
            R_dcoil_vd = (t_oc_base*1.d-3)/(k_mat*A_dcoil_vd(i))+1/(ht_N(i)*A_dcoil_vd(i))
        else
            R_dcoil_vd = 1.0/(ht_dcoil_vd(i)*A_dcoil_vd(i))+(t_oc_base*1.d-3)/(k_mat*A_dcoil_vd(i))+1.0/(ht_N(i)*A_dcoil_vd(i))
        end if
            
        ! --- For heat transfer rate in vertical direction
        Q_schc_vd = (T_sc(i)-T_room)/(omega_1*R_schc_vd)! (T_sc(i)-T_hc)/(omega_1*R_schc_vd)
        Q1_rohc_vd = (T_roller_piston-T_oil)/(omega_1*R1_rohc_vd)
        Q2_rohc_vd = (T_oil-T_room)/(omega_1*R2_rohc_vd) !(T_oil-T_hc)/(omega_1*R2_rohc_vd)
        Q_dchc_vd= (T_dc(i)-T_room)/(omega_1*R_dchc_vd) ! (T_dc(i)-T_hc)/(omega_1*R_dchc_vd)
        !Q1_rohcsc_vd(i) = (T_roi(i)-T_oil)/(omega_1*R1_rohcsc_vd(i))
        !Q2_rohcsc_vd(i) = (T_oil-T_hc)/(omega_1*R2_rohcsc_vd(i))
        !Q1_rohcdc_vd(i)= (T_roi(i)-T_oil)/(omega_1*R1_rohcdc_vd(i))
        !Q2_rohcdc_vd(i)= (T_oil-T_hc)/(omega_1*R2_rohcdc_vd(i))
        
            
        Q_scoil_vd = (T_sc(i)-T_resoil)/(omega_1*R_scoil_vd)
        Q1_rooil_vd = (T_roller_piston-T_oil)/(omega_1*R1_rooil_vd)
        Q2_rooil_vd = (T_oil-T_resoil)/(omega_1*R2_rooil_vd)
        Q_dcoil_vd = (T_dc(i)-T_resoil)/(omega_1*R_dcoil_vd)
        !Q1_rooilsc_vd(i) = (T_roi(i)-T_oil)/(omega_1*R1_rooilsc_vd(i))
        !Q2_rooilsc_vd(i) = (T_oil-T_resoil)/(omega_1*R2_rooilsc_vd(i))
        !Q1_rooildc_vd(i) = (T_roi(i)-T_oil)/(omega_1*R1_rooildc_vd(i))
        !Q2_rooildc_vd(i) = (T_oil-T_resoil)/(omega_1*R2_rooildc_vd(i))
        
        !Q_roller(i) = Q_scro + Q_dcro - Q1_rohc_vd(i) - Q1_rooil_vd(i)
        !deltaT_roller(i) = (Q_roller(i)*omega_1) / (444.0*pi*((15.d-3)**2)*l_com*1.d-3*rho_mat)
        
        ! ----------------------------------------
        !  Net Total rate of heat transfer 
        ! ----------------------------------------
        dqdtheta_sc(i) = Q_dcsc - Q_schc - Q_scro - Q_schc_vd - Q_scoil_vd       ! [J/rad] or [J] Net Heat Transfer rate of suction chamber
        dqdtheta_dc(i) = - Q_dcsc - Q_dchc - Q_dcro - Q_dchc_vd - Q_dcoil_vd     ! [J/rad] or [J] Net Heat Transfer rate of discharge chamber
        dqdtheta_hc(i) = Q_schc + Q_dchc ! + Q_schc_vd + Q_dchc_vd + Q2_rohc_vd !Q2_rohcsc_vd(i) + Q2_rohcdc_vd(i)     ! [J/rad] or [J] Net Heat Transfer rate of housing chamber
        dqdtheta_resoil(i) = Q_scoil_vd + Q2_rooil_vd + Q_dcoil_vd !+ Q2_rooilsc_vd(i) + Q2_rooildc_vd(i) + Q_dcoil_vd(i)      ! ! [J/rad] or [J] Net Heat Transfer rate of oil reservoir/oil sump
        dqdtheta_oil(i) = Q1_rohc_vd - Q2_rohc_vd + Q1_rooil_vd - Q2_rooil_vd       ! [J/rad] or [J] Net Heat dissipation by oil (+ve means heat gain by oil, the heat oil brought out from compressor)
        !dqdtheta_oil(i) = Q1_rohcsc_vd(i) - Q2_rohcsc_vd(i) + Q1_rohcdc_vd(i) - Q2_rohcdc_vd(i) + Q1_rooilsc_vd(i) - Q2_rooilsc_vd(i) + Q1_rooildc_vd(i) - Q2_rooildc_vd(i)
        
        ! ------------------------------------
        ! For exergy use
        ! ------------------------------------
        Q_dcsc_ex(i) = Q_dcsc
        Q_schc_ex(i) = Q_schc
        Q_scro_ex(i) = Q_scro
        Q_schc_vd_ex(i) = Q_schc_vd
        Q_scoil_vd_ex(i) = Q_scoil_vd
        Q_dchc_ex(i) = Q_dchc
        Q_dcro_ex(i) = Q_dcro
        Q_dchc_vd_ex(i) = Q_dchc_vd
        Q_dcoil_vd_ex(i) = Q_dcoil_vd
        
830 continue
        
    print *, ' ---------------------------------------------------------------------------- '
    print '(2x,A,F15.4,A)', 'Avrg. Heat Transfer Rate in Suction Chamber      = ', sum(dqdtheta_sc)/(no_data+1)*omega_1, ' W'
    print '(2x,A,F15.4,A)', 'Avrg. Heat Transfer Rate in Compression Chamber  = ', sum(dqdtheta_dc)/(no_data+1)*omega_1, ' W'
    print '(2x,A,F15.4,A)', 'Avrg. Heat Transfer Rate in Housing Chamber      = ', sum(dqdtheta_hc)/(no_data+1)*omega_1, ' W'
    print '(2x,A,F15.4,A)', 'Avrg. Heat Transfer Rate in Oil Sump             = ', sum(dqdtheta_resoil)/(no_data+1)*omega_1, ' W'
    print '(2x,A,F15.4,A)', 'Avrg. Heat Transfer Rate in Oil lubricant        = ', sum(dqdtheta_oil)/(no_data+1)*omega_1, ' W'
    print *, ' ---------------------------------------------------------------------------- '
    print *, ' '
        ! -------------------------------------------------------
        ! For checking values, uncomment this parts
        ! -------------------------------------------------------
    
!do 890 i = 1, no_data+data_step, data_step
!        
!        write(121,2983) T_sc(i), T_dc(i), T_hc
!        write(122,2983) T_ocsc(i), T_ocdc(i), T_rosc(i), T_rodc(i), T_roller(i)
!        write(123,2983) T_uocsc(i), T_locsc(i), T_uocdc(i), T_locdc(i)
!        write(124,2983) dqdtheta_sc(i), dqdtheta_dc(i), dqdtheta_hc(i), dqdtheta_resoil(i), dqdtheta_oil(i)
!        write(125,2983) Q_schc(i), Q_scro(i), Q_dcsc(i), Q_dchc(i), Q_dcro(i)
!        write(126,2983) Q_scoil_vd(i), Q2_rooil_vd(i), Q_dcoil_vd(i), Q1_rooil_vd(i)
!        write(127,2983) Q_roller(i), deltaT_roller(i), T_roller(i)
!        
!        write(131,2983) U_ocsc_ocsc(i), U_ocsc_sc(i), U_ocsc_hc(i), U_ocsc_ocdc(i), U_ocsc_uocsc(i), U_ocsc_locsc(i)
!        write(132,2983) U_ocdc_ocdc(i), U_ocdc_dc(i), U_ocdc_hc(i), U_ocdc_ocsc(i), U_ocdc_uocdc(i), U_ocdc_locdc(i)
!        write(133,2983) U_rosc_rosc(i), U_rosc_sc(i), U_rosc_roller(i), U_rosc_rodc(i), U_rosc_uocsc(i), U_rosc_locsc(i)
!        write(134,2983) U_rodc_rodc(i), U_rodc_dc(i), U_rodc_roller(i), U_rodc_rosc(i), U_rodc_uocdc(i), U_rodc_locdc(i)
!        write(135,2983) U_roller_roller(i), U_roller_rosc(i), U_roller_rodc(i)
!        write(136,2983) U_uocsc_uocsc(i), U_uocsc_sc(i), U_uocsc_hc(i), U_uocsc_rosc(i), U_uocsc_ocsc(i), U_uocsc_uocdc(i)
!        write(137,2983) U_locsc_locsc(i), U_locsc_sc(i), U_locsc_resoil(i), U_locsc_rosc(i), U_locsc_ocsc(i), U_locsc_locdc(i)
!        write(138,2983) U_uocdc_uocdc(i), U_uocdc_dc(i), U_uocdc_hc(i), U_uocdc_rodc(i), U_uocdc_ocdc(i), U_uocdc_uocsc(i)
!        write(139,2983) U_locdc_locdc(i), U_locdc_dc(i), U_locdc_resoil(i), U_locdc_rodc(i), U_locdc_ocdc(i), U_locdc_locsc(i)
!        
!890 continue        
        
    
201 format (10f12.5)
2982 format (F25.4, 14ES25.6)  
2983 format (14ES25.6) 
endsubroutine
    
  