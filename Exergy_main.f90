subroutine exergy_main(theta_1, v_com, v_suc, p_scv, p_dcv, t_scv, t_dcv, s_scv, s_dcv, h_scv, h_dcv, u_scv, u_dcv, m_scv, m_dcv, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, dqdtheta_sc, dqdtheta_dc, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex, L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s, L_lip_seal)
    !implicit none
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1, ncmax=20
    dimension z(ncmax),x(ncmax),y(ncmax)
    include "var_operational_parameter.f90"
    include "var_simulation_parameter.f90"
    include "var_physical_constants.f90"
    include "var_operating_fluid_condition.f90"
    include "var_compressor_evaluation.f90"
    include "var_exergy.f90"
    integer i
    common/fluid_info/wm
    ! -------- Input
    ! Standard atmosphere condition
    !double precision, parameter :: T_0 = 298.0, p_0 = 101.325
    ! Variables from geometric
    double precision, dimension (1:no_data+1) :: theta_1
    ! Variables from thermodynamic and residual mass
    double precision, dimension (1:no_data+1) :: v_com, v_suc
    double precision, dimension (1:no_data+1) :: p_scv, p_dcv, t_scv, t_dcv, s_scv, s_dcv, h_scv, h_dcv, u_scv, u_dcv, m_scv, m_dcv
    double precision, dimension (1:no_data+1) :: dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do
    double precision mass_residual_dead, h_residual_dead        ! residual mass caused by dead volume
    common/reexpand/mass_residual_dead, h_residual_dead
    ! Variables from power model (input power, individual losses)
    double precision, dimension (1:no_data+1) :: L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s, L_lip_seal       ! Losses definition

    ! Variables from heat transfer
    double precision, dimension (1:no_data+1) ::  dqdtheta_sc, dqdtheta_dc, Q_dcsc_ex, Q_schc_ex, Q_scro_ex, Q_schc_vd_ex, Q_scoil_vd_ex, Q_dchc_ex, Q_dcro_ex, Q_dchc_vd_ex, Q_dcoil_vd_ex
    double precision T_hc, T_oil, T_resoil
    double precision T_roller_piston, T_vanehousing
    common/heat_temperature/T_hc, T_oil, T_resoil, T_roller_piston, T_vanehousing
    ! ---------- Output
    double precision, dimension (1:no_data+1) :: I_thr_scv, I_thr_dcv, I_thr_leak, I_ht_scv, I_ht_dcv
    double precision, dimension (1:no_data+1) :: I_press_scv, I_press_dcv, I_tempmx_scv, I_tempmx_dcv
    double precision, dimension (1:no_data+1) :: I_ef_vh, I_s_vh, I_f_vs, I_ef_ro, I_lub, I_bear_s, I_lip_seal         ! irreversibilities for friction loss
    double precision I_dcsc_ex, I_schc_ex, I_scro_ex, I_schc_vd_ex, I_scoil_vd_ex, I_dchc_ex, I_dcro_ex, I_dchc_vd_ex, I_dcoil_vd_ex    ! Irreversibilities for suction and discharge control volume (heat)
    !double precision I_reexpand, I_friction_total, I_thr_total, I_heat_total
    !double precision exergy_sup, exergy_destroyed
    !common/exergy_var/exergy_sup, exergy_destroyed, I_reexpand, I_friction_total, I_thr_total, I_heat_total
    
    do i = 1, no_data+1
        ! -------------------------------
        ! --- Throttling loss
        ! Throttling loss alone is dmdt*T_0*(s_e - s_i)
        ! Temperature mixing is dmdt*T_0*(s_cv - s_e - (h_cv - h_i)/T_cv)
        ! combined will be whole throttling loss
        ! -------------------------------
        ! Call refprop routine for the immediate entropy 
        p1_suc = p_scv(i)   ! [kPa]
        h1_mol_suc = h_suc/1000. * wm  ! convert to [J/mol] for input
        call PHFLSH (p1_suc,h1_mol_suc,z,t,D,Dl,Dv,x,y,q,e,s_e_suc,cv,cp,w,ierr,herr)       ! PHFLSH
        p2_disc = p_disc ! [kPa]
        h2_mol_disc = h_dcv(i)/1000.0*wm  ! convert to [J/mol] for input
        call PHFLSH (p2_disc,h2_mol_disc,z,t,D,Dl,Dv,x,y,q,e,s_e_disc,cv,cp,w,ierr,herr)       ! PHFLSH
        s_e_suc = s_e_suc*1000. / wm
        s_e_disc = s_e_disc*1000. / wm

        I_thr_scv(i) = abs((dmdtheta1_si(i) - dmdtheta1_so(i))*T_0*(s_scv(i) - s_suc - (h_scv(i) - h_suc)/t_scv(i))*omega_1)       ! [W]   whole throttling lost at suction port (combined with mixing) 
        I_thr_dcv(i) = abs((dmdtheta1_do(i) - dmdtheta1_di(i))*T_0*(s_disc - s_dcv(i) - (h_disc - h_dcv(i))/t_dcv(i))*omega_1)       ! [W]  whole throttling lost at discharge port (combined with mixing)
        I_press_scv(i) = abs((dmdtheta1_si(i) - dmdtheta1_so(i))*T_0*(s_e_suc - s_suc)*omega_1)     ! [W] throttling loss (only) at suc
        I_press_dcv(i) = abs((dmdtheta1_do(i) - dmdtheta1_di(i))*T_0*(s_e_disc - s_dcv(i))*omega_1) ! [W] throttling loss (only) at disc
        I_tempmx_scv(i) = abs((dmdtheta1_si(i) - dmdtheta1_so(i))*T_0*(s_scv(i) - s_e_suc - (h_scv(i) - h_suc)/t_scv(i))*omega_1)       ! [W] mixing loss at suction
        I_tempmx_dcv(i) = abs((dmdtheta1_do(i) - dmdtheta1_di(i))*T_0*(s_disc - s_e_disc - (h_disc - h_dcv(i))/t_dcv(i))*omega_1)       ! [W] mixing loss at disc
        I_thr_leak(i) = abs(dmdtheta1_leak_s(i)*T_0*(s_scv(i) - s_dcv(i) - (h_scv(i) - h_dcv(i))/t_scv(i))*omega_1)          ! [W] throttling lost of leakage  (total)
        ! ------------------------------
        ! --- Individual heat transfer irreversibility
        I_dcsc_ex = abs(Q_dcsc_ex(i)*T_0*(1./t_scv(i) - 1./t_dcv(i)))
        I_schc_ex = abs(Q_schc_ex(i)*T_0*(1./t_scv(i) - 1./T_hc))
        I_scro_ex = abs(Q_scro_ex(i)*T_0*(1./t_scv(i) - 1./T_roller_piston))
        I_schc_vd_ex = abs(Q_schc_vd_ex(i)*T_0*(1./t_scv(i) - 1./T_hc))
        I_scoil_vd_ex = abs(Q_scoil_vd_ex(i)*T_0*(1./t_scv(i) - 1./T_resoil))
        
        I_dchc_ex = abs(Q_dchc_ex(i)*T_0*(1./t_dcv(i) - 1./T_hc))
        I_dcro_ex = abs(Q_dcro_ex(i)*T_0*(1./t_dcv(i) - 1./T_roller_piston))
        I_dchc_vd_ex = abs(Q_dchc_vd_ex(i)*T_0*(1./t_dcv(i) - 1./T_hc))
        I_dcoil_vd_ex = abs(Q_dcoil_vd_ex(i)*T_0*(1./t_dcv(i) - 1./T_hc))
        ! --- Heat transfer irreversibility in chambers
        I_ht_scv(i) = (I_dcsc_ex + I_schc_ex + I_scro_ex + I_schc_vd_ex + I_scoil_vd_ex)*omega_1  ! [W] total heat transfer irreversibility in suction chamber
        I_ht_dcv(i) = (I_dcsc_ex + I_dchc_ex + I_dcro_ex + I_dchc_vd_ex + I_dcoil_vd_ex)*omega_1    ! [W] total heat transfer irreversibility in discharge chamber
        ! ------------------------------------
        ! --- Friction
        I_ef_vh(i) = L_ef_vh(i)*T_0/T_vanehousing   ! [W] Vane housing endface frictional loss
        I_s_vh(i) = L_s_vh(i)*T_0/T_vanehousing     ! [W] Vane housing side frictional loss (body rubbing)
        I_f_vs(i) = L_f_vs(i)*T_0/T_vanehousing     ! [W] Vane housing vane side frictional loss
        I_ef_ro(i) = L_ef_ro(i)*T_0/T_roller_piston  ! [W] rotor endface frictional loss
        I_lub(i) = L_lub(i)*T_0/T_roller_piston      ! [W] eccentric rubbing
        I_bear_s(i) = P_bear_s(i)*T_0/T_oil         ! [W] Bearing frictional loss
        I_lip_seal(i) = L_lip_seal(i)*T_0/T_oil     ! [W] Lip seal frictional loss
        
        ! ------------------------------------
    enddo
        I_reexpand = m_scv(no_data+1)*(u_scv(no_data+1) - T_0*s_scv(no_data+1)) + p_0*v_com(1)*1.e-6 + mass_residual_dead*(u_dcv(no_data+1) - T_0*s_dcv(no_data+1)) + p_0*v_com(no_data+1)*1.e-6 - m_dcv(1)*(u_dcv(1) - T_0*s_dcv(1)) - p_0*v_com(1)*1.e-6      ! [W] reexpansion loss
        I_friction_total = sum(I_ef_vh)/(no_data+1) + sum(I_s_vh)/(no_data+1) + sum(I_f_vs)/(no_data+1) + sum(I_ef_ro)/(no_data+1) + sum(I_lub)/(no_data+1) + sum(I_bear_s)/(no_data+1) + sum(I_lip_seal)/(no_data+1)    ! [W] total exergy destroyed by friction
        I_heat_total = sum(I_ht_scv)/(no_data+1) +  sum(I_ht_dcv)/(no_data+1)       ! [W] total exergy destroyed by heat transfer
        I_thr_only = sum(I_press_scv)/(no_data+1) + sum(I_press_dcv)/(no_data+1)    ! [W] throttling only
        I_mix_only = sum(I_tempmx_scv)/(no_data+1) + sum(I_tempmx_dcv)/(no_data+1)  ! [W] Fluid mixing only
        !I_thr_total = sum(I_thr_scv)/(no_data+1) + sum(I_thr_dcv)/(no_data+1) + sum(I_thr_leak)/(no_data+1) ! [W] total exergy destroyed by throttling
        I_thr_total = I_thr_only + I_mix_only   ! sum of throttling loss and mixing loss [W]
        exergy_destroyed = I_reexpand + I_friction_total + I_thr_total + I_heat_total       ! [W] Total exergy destroyed
        exergy_sup_fluid = mass_total*(h_disc - T_0*(s_disc) - h_suc + T_0*(s_suc))     ! [W] Exergy transferred to fluid that being compressed
        exergy_sup_total = exergy_sup_fluid + I_reexpand + I_friction_total + I_thr_total + I_heat_total  ! [W] Total exergy supplied
        !exergy_sup_wo_fluid = mass_total*(h_disc - T_0*(s_disc) - h_suc + T_0*(s_suc)) + I_reexpand + I_friction_total + I_thr_total + I_heat_total  ! [W] Total exergy supplied (without fluid supply, Motor work only)
        eff_2nd = 1.0 - (I_reexpand + I_friction_total + I_thr_total + I_heat_total)/exergy_sup_total     ! 2nd law efficiency
        !eff_2nd_wo_fluid = 1.0 - (I_reexpand + I_friction_total + I_thr_total + I_heat_total)/exergy_sup_wo_fluid
        
        ! ---------------------------------------------
        ! Write file titles and array
        ! ---------------------------------------------
        write(161,2981), "Degree", "I_press_scv[W]", "I_press_dcv[W]", "I_tempmx_scv[W]", "I_tempmx_dcv[W]", "I_thr_scv[W]", "I_thr_dcv[W]", "I_thr_leak[W]", "I_ht_scv[W]", "I_ht_dcv[W]", "I_ef_vh[W]", "I_s_vh[W]", "I_f_vs[W]", "I_ef_ro[W]", "I_lub[W]", "I_bear_s[W]", "I_lip_seal[W]"
        do i = 1, no_data+data_step, data_step
            write(161,2982), theta_1(i)*180.0/pi, I_press_scv(i), I_press_dcv(i), I_tempmx_scv(i), I_tempmx_dcv(i), I_thr_scv(i), I_thr_dcv(i), I_thr_leak(i), I_ht_scv(i), I_ht_dcv(i), I_ef_vh(i), I_s_vh(i), I_f_vs(i), I_ef_ro(i), I_lub(i), I_bear_s(i), I_lip_seal(i)
        enddo
        ! ---------------------------------------------
        ! Write on screen
        ! ---------------------------------------------
        print *, " ---------------------------------------------------------------------------- "
        print *, " Exergy analysis (Second Law analysis) "
        print *, " ---------------------------------------------------------------------------- "
        write (6,2990) "Exergy Destroyed by Throttling only             I_thr_only          = ", I_thr_only, " W"
        write (6,2990) "Exergy Destroyed by Fluid Mixing only           I_mix_only          = ", I_mix_only, " W"
        write (6,2990) "Total Mean Exergy Destroyed Rate (Friction)     I_friction_total    = ", I_friction_total, " W"
        write (6,2990) "Total Mean Exergy Destroyed Rate (Throttling)   I_thr_total         = ", I_thr_total, " W"
        write (6,2990) "Total Mean Exergy Destroyed Rate (Heat)         I_heat_total        = ", I_heat_total, " W"
        write (6,2990) "Total Mean Exergy Destroyed Rate (Reexpansion)  I_reexpand          = ", I_reexpand, " W"
        write (6,2990) "Total Mean Exergy Rate Transferred to fluid     exergy_sup_fluid    = ", exergy_sup_fluid, " W"
        !write (6,2990) "Total Mean Exergy Supplied Rate (without fluid) exergy_sup_wo_fluid = ", exergy_sup_wo_fluid, " W"
        write (6,2990) "Total Mean Exergy Supplied Rate (with fluid)    exergy_sup_total    = ", exergy_sup_total, " W"
        write (6,2990) "Total Mean Exergy Destroyed Rate                exergy_destroyed    = ", exergy_destroyed, " W"
        write (6,2990) "Second Law efficiency                           eff_2nd             = ", eff_2nd*100.0, " %"
        !write (6,2990) "Second Law efficiency (without fluid)           eff_2nd_wo_fluid    = ", eff_2nd_wo_fluid*100.0, " %"
        write (6,*) ' '
        ! ---------------------------------------------
        ! Write in overview file
        ! ---------------------------------------------
        write (113,*) " -------------------------------------------------------------------- "
        write (113,*) " Exergy analysis (2nd Law analysis) "
        write (113,*) " -------------------------------------------------------------------- "
        write (113,2990) "Exergy Destroyed by Throttling only             I_thr_only          = ", I_thr_only, " W"
        write (113,2990) "Exergy Destroyed by Fluid Mixing only           I_mix_only          = ", I_mix_only, " W"
        write (113,2990) "Total Mean Exergy Destroyed Rate (Friction)     I_friction_total    = ", I_friction_total, " W"
        write (113,2990) "Total Mean Exergy Destroyed Rate (Throttling)   I_thr_total         = ", I_thr_total, " W"
        write (113,2990) "Total Mean Exergy Destroyed Rate (Heat)         I_heat_total        = ", I_heat_total, " W"
        write (113,2990) "Total Mean Exergy Destroyed Rate (Reexpansion)  I_reexpand          = ", I_reexpand, " W"
        write (113,2990) "Total Mean Exergy Rate Transferred to fluid     exergy_sup_fluid    = ", exergy_sup_fluid, " W"
        !write (113,2990) "Total Mean Exergy Supplied Rate (without fluid) exergy_sup_wo_fluid = ", exergy_sup_wo_fluid, " W"
        write (113,2990) "Total Mean Exergy Supplied Rate (with fluid)    exergy_sup_total    = ", exergy_sup_total, " W"
        write (113,2990) "Total Mean Exergy Destroyed Rate                exergy_destroyed    = ", exergy_destroyed, " W"
        write (113,2990) "Second Law efficiency                           eff_2nd             = ", eff_2nd*100.0, " %"
        !write (113,2990) "Second Law efficiency (without fluid)           eff_2nd_wo_fluid    = ", eff_2nd_wo_fluid*100.0, " %"
        write (113,*) ' '
        ! ---------------------------------------------
        ! Write to file
        ! ---------------------------------------------
        !write (113,*) " -------------------------------------------------------------------- "
        !write (113,*) " Actual Compressor Performance "
        !write (113,*) " -------------------------------------------------------------------- "
        !write (113,2990) "Total Volume                          vol_total           = ", vol_total/1000., " cc or cm3"
        !write (113,2990) "Residual mass (Dead volume)           mass_residual_dead  = ", mass_residual_dead*1000, " g"
        !write (113,2990) "Avrg. Leakage Mass Flow               avrg_leakage_mass   = ", avrg_leakage_mass*1000, " g/s"
        !write (113,2990) "Total Actual Mass Flow Rate           mass_total          = ", mass_total, " kg/s"
        !write (113,2990) "Cubic Feet per Minute (CFM)                               = ", mass_total/rho_disc*35.3147*60, " CFM"
        !
        !write (113,2990) "Cooling Capacity                          q_capacity      = ", q_capacity, " W"
        !write (113,2990) "Indicated Work for NVC w/o heat & leakage P_ind           = ", P_ind, " W"
        !write (113,2990) "Indicated Work                            P_ind_hl        = ", P_ind_hl, " W"
        
2981 format (20A25)
2982 format (F25.4, 18ES25.6)    
2983 format (14ES25.6) 
2989 format (2x,A,F8.4,A)
2990 format (2x,A,f12.4,A)
endsubroutine