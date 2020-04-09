subroutine power_m(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, h_scv, h_dcv, dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do, dvdtheta1_s, dvdtheta1_d, dlvdt, dgammadt_1, F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_cx, F_cy, F_resultant, eccr, att, T_inertia_ro, T_inertia_vh, T_com_C, L_f_vs, L_ef_vh, L_s_vh, L_ef_ro, L_lub, P_bear_s, L_lip_seal)
    implicit none
    include 'var_simulation_parameter.f90'
    include "var_operational_parameter.f90"
    include 'var_physical_constants.f90'
    include 'var_main_dimensions.f90'
    include 'var_kinematics.f90'
    include "var_geometrical.f90"
    include "var_geometrical_diff.f90"
    include 'var_compressor_evaluation.f90'
    include "var_dynamic.f90"
    include "var_clearance.f90"
    include "var_indicated_work.f90"
    include "var_operating_fluid_condition.f90"
    ! ---------- for simulation ------------- !
    integer i, journal_opt, heat_opt, leakage_opt
    common/model_opt/journal_opt, heat_opt, leakage_opt
    integer optimization_opt
    common/routine_opt/optimization_opt
    ! ---------- input variables ------------ !
    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id   ! suction and compression chamber pressure (ideal)
    double precision, dimension (1:no_data+1) :: p_scv, p_dcv, h_scv, h_dcv   ! suction chamber and compression chamber properties (actual)
    double precision, dimension (1:no_data+1) :: dmdtheta1_si, dmdtheta1_so, dmdtheta1_leak_s, dmdtheta1_di, dmdtheta1_do
    double precision, dimension (1:no_data+1) :: eccr, att      ! journal bearing
    ! ---------- Output variables are in the file var_power.f90 ---------- !
    include "var_power.f90"
    ! ---------- dummy variables ------------ !
    double precision L_ef_vh_I, L_ef_vh_II, A_ratio_vh
    double precision vel_avrg
    
    
    ! -------------------------------------------
    ! Indicated Power (with heat transfer and leakage model)
    ! -------------------------------------------
    call indicated_work(v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, dvdtheta1_s, dvdtheta1_d, P_ind_hl, P_ind_id, P_loss_suc_hl, P_loss_disc_hl, P_comp_loss_hl)
    
    ! -------------------------------------------
    ! valve loss 
    ! -------------------------------------------
    P_valve_loss = P_loss_suc_hl + P_loss_disc_hl       ! total valve loss due to discharge and suction
    ! ----- Dummy calculation -----------
    A_ratio_vh = 1 - (l_v_ro + l_vs_ro)*w_v_ro/(pi*r_vh**2 + (2*w_vs_ro + w_v_ro)*l_vh)     ! Area ratio for vane housing
    vel_avrg = r_roi*1.e-3*omega_1 ! (e + r_roi + r_shaft)*1.e-3*omega_1/2.   ! velocity [m/s], velocity calculate to the tip of the roller, and the other side
    !T_lub = 2.*pi*(r_ecc*1.e-3)*(l_ecc*1.e-3)*miu_oil*vel_avrg/cl_rad_roll*r_ecc*1.e-3        ! [Nm] Area*Stress*moment_arm, assume constans moment arm, torque due to lubricant dragging friction
    

    do 198 i = 1, no_data+1
        ! -----------------------------------------------------------------
        ! Total 5 kinds of frictional losses, 
        ! L_f_vs -- vane side friction
        ! L_ef_vh -- vane housing endface friction
        ! L_s_vh -- vane housing side friction
        ! L_ef_ro -- rotor endface
        ! L_lub -- lubricant layer friction (roller and rotor) -- prior to L_lub, T_lub is calculated first
        ! ------------ Vane housing friction (endface and side ) ---------- !
        L_ef_vh_I = 0.5*pi*miu_oil*(r_vh*1.e-3)**4/cl_upend*dgammadt_1(i)**2 + 0.5*pi*miu_oil*(r_vh*1.e-3)**4/cl_lowend*dgammadt_1(i)**2      ! [W] Dummy -- frictional loss of vane housing Section I
        L_ef_vh_II = pi*miu_oil*(l_vh*1.e-3)/cl_upend*(2*w_vs_ro + w_v_ro)*1.e-3*((r_vh + 0.5*l_vh)*1.e-3)**2*dgammadt_1(i)**2 + pi*miu_oil*(l_vh*1.e-3)/cl_lowend*(2*w_vs_ro + w_v_ro)*1.e-3*((r_vh + 0.5*l_vh)*1.e-3)**2*dgammadt_1(i)**2       ! [W] Dummy -- frictional loss of vane housing Section II
        L_ef_vh(i) = A_ratio_vh*(L_ef_vh_I + L_ef_vh_II)            ! [W] endface frictional loss of vane housing 
        L_ef_vh(i) = abs(L_ef_vh(i))                ! [W] only absolute value
        L_s_vh(i) = r_vh*1.e-3*coef_vhs*sqrt(F_vhoc_n(i)**2 + F_vhoc_t(i)**2)*dgammadt_1(i)     ! [W] vane housing side frictional loss (the head)
        L_s_vh(i) = abs(L_s_vh(i))       ! [W]  take only absolute value
        ! ------------ Vane side friction ------------------- !
        L_f_vs(i) = abs(coef_vs*(F_1_n(i)) + (F_2_n(i))*dlvdt(i)*1.e-3)      ! [W] vane side frictional loss (abs)
        ! ------------ Rotor friction ----------------------- !
        L_ef_vro(i) = miu_oil/cl_upend*((l_v_ro*w_v_ro*1.e-6*(dlvdt(i)*1.e-3)**2) + (l_v_ro*w_v_ro*1.e-6*((r_vh + l_v(i) - 0.5*l_v_ro)*1.e-3)**2*dgammadt_1(i)**2)) + miu_oil/cl_lowend*((l_v_ro*w_v_ro*1.e-6*(dlvdt(i)*1.e-3)**2) + (l_v_ro*w_v_ro*1.e-6*((r_vh + l_v(i) - 0.5*l_v_ro)*1.e-3)**2*dgammadt_1(i)**2))   ! [W]  rotor vane endface frictional loss
        L_ef_vro(i) = abs(L_ef_vro(i))
        L_ef_rohc(i) = pi*miu_oil/cl_upend*((r_ro**2 - r_roi**2)*dlvdt(i)**2*1.e-12 + (0.5*(r_ro**4 - r_roi**4) + (r_vh + l_v(i) + r_ro)**2*(r_ro**2 - r_roi**2))*1.e-12*dgammadt_1(i)**2) + pi*miu_oil/cl_lowend*((r_ro**2 - r_roi**2)*dlvdt(i)**2*1.e-12 + (0.5*(r_ro**4 - r_roi**4) + (r_vh + l_v(i) + r_ro)**2*(r_ro**2 - r_roi**2))*1.e-12*dgammadt_1(i)**2)
        L_ef_rohc(i) = abs(L_ef_rohc(i))        ! [W] rotor body (cylinder) endface frictional loss
        L_ef_ro(i) = L_ef_vro(i) + L_ef_rohc(i)         ! Rotor total endface frictional loss
        
        ! ------------ Friction between eccentric and rotor ------------- !
        !L_lub(i) = abs(T_lub*omega_1)
        !T_ecc(i) = miu_oil*(omega_1 - dgammadt_1(i))*r_ecc**3*l_ecc*1.e-12*pi*( (2.0 + eccr(i)) / (1.0 + eccr(i)) ) / ( cl_rad_roll*sqrt(1.0-eccr(i)**2))  +  cl_rad_roll*eccr(i)*F_resultant(i)*sin(att(i))/2.    ! [Nm] journal bearing analysis
        !T_ecc(i) = 0.3238*2.0*pi*r_roi**2*l_ecc*miu_oil*1.d-9*((e + r_roi + r_shaft)*1.d-3*(omega_1) - (r_roi*1.d-3*dgammadt_1(i)))/cl_rad_roll + 0.6762*F_resultant(i)*0.1*r_roi*1.d-3      ! Y.D. simplified torque
        !T_ecc(i) = 0.3238*2.0*pi*r_roi**3*l_ecc*miu_oil*1.d-12*(omega_1 - dgammadt_1(i))/cl_rad_roll + 0.6762*abs(F_resultant(i))*0.1*r_roi*1.d-3 ! FOR PROTOTYPE VERSION
        T_ecc(i) = 2.0*pi*r_roi**3*l_ecc*miu_oil*1.d-12*(omega_1 - dgammadt_1(i))/cl_rad_roll ! FOR REPORT VERSION
        !T_ecc(i) = 0.3238*2.0*pi*(l_ecc*1.d-3)*miu_oil/cl_rad_roll*(((e + r_ecc)**3 - r_shaft**3)*1.d-9*omega_1/3.0 - ((e + r_ecc)**2 - r_shaft**2)*r_roi*1.d-9*dgammadt_1(i)/2.0) + 0.6762*F_resultant(i)*0.1*r_ecc*1.d-3       ! Y.D. derived torque 
        !T_ecc(i) = F_resultant(i)*0.16*0.5*r_ecc*1.d-3
        L_lub(i) = abs(T_ecc(i)*(omega_1 - dgammadt_1(i)))       ! [W] frictional loss due to rubbing surface between roller and rotor  ! FOR REPORT VERSION
        !L_lub(i) = 0.3238*2.0*pi*r_roi**3*l_ecc*miu_oil*1.d-12*(omega_1 - dgammadt_1(i))**2/cl_rad_roll + 0.6762*abs(F_resultant(i))*0.1*r_roi*1.d-3*omega_1 ! FOR PROTOTYPE VERSION
        
        ! ---------- Friction between eccentric endface -------------- !
        L_ef_ecc(i) = 0.5*pi*miu_oil*omega_1**2/cl_eccef_up*(r_ecc**4 - r_shaft**4)*1.d-12 + 0.5*pi*miu_oil*omega_1**2/cl_eccef_low*(r_ecc**4 - r_shaft**4)*1.d-12
        ! ------------ Loss due to leakage --------------------------- !
        L_leakage(i) = dmdtheta1_leak_s(i)*h_dcv(i)*omega_1     ! power is used to compress the gas but it leak to the next cycle, and may flow out from the compressor
        ! ------------ Friction due to lip seal ---------------------- !
        !L_lip_seal(i) = 2.0*pi*coef_circ_lip*(r_shaft*1.d-3)**2*omega_1     ! [W] according to Muller and Nau
        L_lip_seal(i) = 0.0
        
        ! ===================================================
        ! Total Instantaneous power, total frictional power loss, 
        ! rotor inertia, vane housing inertia, compression power (individual)
        ! calcualte power into the system first
        ! then move the centre to C
        ! ------------------------------------------------------
        if (journal_opt == 1) then
            P_bear_s(i) = 2.0*r_bearing*1.e-3*omega_1*(miu_oil*omega_1*r_bearing**2*l_bearing*1.e-9*pi*( (2.0 + eccr(i)) / (1.0 + eccr(i)) ) / ( cl_bear_s*sqrt(1.0-eccr(i)**2)))  +  2.0*r_bearing*1.e-3*omega_1*(cl_bear_s*eccr(i)*0.5*F_resultant(i)*sin(att(i))/(2*r_bearing*1.e-3))
            !P_bear_s(i) = 1.3238*r_bearing*1.e-3*omega_1*(miu_oil*omega_1*r_bearing**2*l_bearing*1.e-9*pi*( (2.0 + eccr(i)) / (1.0 + eccr(i)) ) / ( cl_bear_s*sqrt(1.0-eccr(i)**2)))  +  1.3238*r_bearing*1.e-3*omega_1*(cl_bear_s*eccr(i)*0.5*F_resultant(i)*sin(att(i))/(2*r_bearing*1.e-3)) + 0.6762*0.5*F_resultant(i)*0.05*omega_1*(r_bearing*1.d-3)
            !P_bear_s(i) = 2.0*0.5*F_resultant(i)*0.05*omega_1*(r_bearing*1.d-3)        
            !P_bear_s(i) = 2*omega_1**2*pi*(l_bearing*1.d-3)*(r_bearing*1.d-3)/(cl_bear_s)**4        ! no lubrication
        else
            P_bear_s(i) = 0.0
        endif
        P_f_loss(i) = L_ef_vh(i) + L_s_vh(i) + L_f_vs(i) + L_ef_ro(i) + L_lub(i) + P_bear_s(i) + L_lip_seal(i) ! + L_ef_ecc(i)     ! [W]    Instantaneous total frictional power loss
        !P_thr_loss(i) = (abs((dmdtheta1_si(i) - dmdtheta1_so(i))*(h_suc - h_scv(i))) + abs((dmdtheta1_di(i) - dmdtheta1_do(i))*(h_dcv(i) - h_disc)))*omega_1  ! [W] throttling loss between suction port to chamber and chamber to discharge port
        P_inertia_ro(i) = T_inertia_ro(i)*dgammadt_1(i)        ! [W] rotor inertia power
        P_inertia_vh(i) = T_inertia_vh(i)*dgammadt_1(i)        ! [W] Vane housing inertia power
        P_indicated(i) = - p_dcv(i)*dvdtheta1_d(i)*1000.0*omega_1 - p_scv(i)*dvdtheta1_s(i)*1000.0*omega_1
        P_com(i) = P_indicated(i) !T_com_C(i)*omega_1     ! [W] Compression power = indicated power (PV area)
        P_inst_total_no_loss(i) = P_inertia_ro(i) + P_inertia_vh(i) + P_indicated(i) !P_com(i)  
        !P_inst_total(i) = P_inertia_ro(i) + P_inertia_vh(i) + P_com(i) + P_f_loss(i) + P_valve_loss ! + P_thr_loss(i) ! + P_valve_loss
        
        ! ---------- Reexpansion loss ------------- !
        if (theta_1(i) > theta_disc_end .and. i .ne. no_data+1) then
            L_reexpansion(i) = - 0.5*(p_dcv(i) + p_dcv(i+1))*dvdtheta1_d(i)*1000.0*omega_1      ! loss to compress the residual mass which is expanded to compression chamber in next cycle
        else
            L_reexpansion(i) = 0.0
        endif
        ! --------- Total Loss ----------------------
        P_inst_total(i) = P_com(i) + P_f_loss(i) !P_inertia_ro(i) + P_inertia_vh(i) + L_reexpansion(i) !+ L_leakage(i) ! + P_thr_loss(i) ! + P_valve_loss     ! compression + frictional + reexpansion
        ! --------- friction torque -------------- !
        T_loss(i) = P_f_loss(i)/omega_1
        
        ! --------- Torque of bearing friction --- !
        T_bear_s(i) = P_bear_s(i)/omega_1
        
        ! --------- Torque of lip seal friction -------- !
        T_lip_seal(i) = L_lip_seal(i)/omega_1
        
        ! --------- Inertia torque (about C) ----- !
        T_inertia_ro(i) = P_inertia_ro(i)/omega_1
        T_inertia_vh(i) = P_inertia_vh(i)/omega_1
        
        ! --------- Compression torque (about C) -- !
        T_com_C(i) = P_com(i)/omega_1
        
        ! --------- Total torque without loss ----- !
        T_total_no_loss(i) = P_inst_total_no_loss(i) / omega_1
        
        ! --------- Total Instantaneous torque ---- !
        T_inst_total(i) = P_inst_total(i)/omega_1       ! [Nm/rad]

198 continue        
        !P_input = 0.d0
        !do i = 1, no_data
        !    P_input = P_input + theta_step*(T_inst_total(i) + T_inst_total(i+1))/2.
        !enddo
        !print*, P_input*freq
        
        ! ---------------- Average individual loss ------------- ! (NOT WRITTING INTO FILE)
        L_avrg_ef_vh = sum(L_ef_vh)/(no_data+1)
        L_avrg_s_vh = sum(L_s_vh)/(no_data+1)
        L_avrg_f_vs = sum(L_f_vs)/(no_data+1)
        L_avrg_ef_ro = sum(L_ef_ro)/(no_data+1)
        L_avrg_lub = sum(L_lub)/(no_data+1)
        L_avrg_ef_ecc = sum(L_ef_ecc)/(no_data+1)
        L_avrg_reexpansion = sum(L_reexpansion)/(no_data+1)
        L_avrg_leakage = sum(L_leakage)/(no_data+1)
        L_avrg_lip_seal = sum(L_lip_seal)/(no_data+1)
        ! ---------------- Average power/power loss ------------ !
        P_input = sum(P_inst_total)/(no_data+1)     ! [W] average total instantaneous power, which is also input power (loss included)
        P_avrg_no_loss = sum(P_inst_total_no_loss)/(no_data+1)      ! [W] Average instantaneous power without loss
        P_avrg_loss = sum(P_f_loss)/(no_data+1)     ! [W] total average loss (friction)
        P_avrg_inertia_ro = sum(P_inertia_ro)/(no_data+1)           ! [W] Average inertial power or rotor
        P_avrg_inertia_vh = sum(P_inertia_vh)/(no_data+1)           ! [W] Average inertia power of vane housing
        P_avrg_com = sum(P_com)/(no_data+1)                         ! [W] Average Compression power
        P_avrg_indicated = sum(P_indicated)/(no_data + 1)           ! [W] Average Indicated power
        P_avrg_bear_s = sum(P_bear_s)/(no_data+1)           ! [W] Average bearing loss
        P_avrg_ecc = L_avrg_lub     ! [W] Average eccentric frictional loss
        !P_avrg_thr_loss = sum(P_thr_loss)/(no_data+1)           ! [W] Average throttling loss
        ! ----------------- Average Torque ------------- !
        T_avrg_no_loss = P_avrg_no_loss/omega_1     ! [Nm] Average torque when no losses
        T_avrg_input = P_input/omega_1              ! [Nm] Average input torque (from motor)
        T_avrg_loss = P_avrg_loss/omega_1           ! [Nm] average torque of frictional losses
        T_avrg_I_ro = P_avrg_inertia_ro/omega_1     ! [Nm] average rotor inertia torque
        T_avrg_I_vh = P_avrg_inertia_vh/omega_1     ! [Nm] average vane housing inertia torque
        T_avrg_com = P_avrg_com/omega_1             ! [Nm] average compression torque
        T_avrg_bear_s = P_avrg_bear_s/omega_1       ! [Nm] average bearing friction torque
        T_avrg_ecc = P_avrg_ecc/omega_1             ! [Nm] Average eccentric friction torque (relative to shaft speed)
        T_avrg_lip_seal = L_avrg_lip_seal/omega_1   ! [Nm] Average torque of Lip Seal friction
        T_peak = maxval(P_inst_total)/omega_1       ! [Nm] Maximum shaft torque
        
        
        ! -------------------------------------- !
        ! Create header for txt file
        ! -------------------------------------- !
        write (90,2981), "Degree","F_vhoc_n[N]","F_vhoc_t[N]","F_1_n[N]","F_2_n[N]","F_cx[N]","F_cy[N]", "F_resultant[N]"    ! version 2.3
        write (91,2981), "Degree","T_ecc[Nm]","T_inertia_ro[Nm]", "T_inertia_vh[Nm]", "T_com_C[Nm]", "T_total_no_loss[Nm]", "T_loss[Nm]", "T_bear_s[Nm]", "T_inst_total[Nm]"
        write (92,2981), "Degree","L_f_vs[W]", "L_ef_vh[W]", "L_s_vh[W]", "L_ef_ro[W]", "L_lub[W]", "L_ef_vro[W]", "L_ef_rohc[W]", "L_lip_seal[W]", "L_reexpansion[W]", "L_ef_ecc[W]"
        write (93,2981), "Degree","P_inst_total[W]", "P_inst_total_no_loss[W]", "P_f_loss[W]", "P_thr_loss[W]","P_inertia_ro[W]", "P_inertia_vh[W]", "P_com[W]", "P_bear_s[W]" 
        ! -----------------------------------------------------------------------------
        ! write data into file "Forces - resultant.txt" & "Torques - individual components.txt"
        ! -----------------------------------------------------------------------------
    do 124 i = 1, no_data+data_step, data_step
        
        !write(90,2982) theta_1(i)*180/pi, F_ox(i), F_oy(i), T_vhro(i), M_o(i), F_1_n(i), F_2_n(i), F_cx(i), F_cy(i), T_roll(i)     ! for 9 unknown case only
        write(90,2982) theta_1(i)*180/pi, F_vhoc_n(i), F_vhoc_t(i), F_1_n(i), F_2_n(i), F_cx(i), F_cy(i), F_resultant(i)     ! for 6 unknown case only
        write(91,2982) theta_1(i)*180/pi, T_ecc(i), T_inertia_ro(i), T_inertia_vh(i), T_com_C(i), T_total_no_loss(i), T_loss(i), T_bear_s(i), T_inst_total(i)
        write(92,2982) theta_1(i)*180/pi, L_f_vs(i), L_ef_vh(i), L_s_vh(i), L_ef_ro(i), L_lub(i), L_ef_vro(i), L_ef_rohc(i), L_lip_seal(i), L_reexpansion(i), L_ef_ecc(i)
        write(93,2982) theta_1(i)*180/pi, P_inst_total(i), P_inst_total_no_loss(i), P_f_loss(i), P_thr_loss(i), P_inertia_ro(i), P_inertia_vh(i), P_com(i), P_bear_s(i)
124 continue      
    
    

    
2981 format (15A25)
2982 format (F25.4, 14ES25.6)    
2983 format (14ES25.6) 
endsubroutine power_m