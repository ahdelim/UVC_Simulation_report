subroutine indicated_work(v_suc, v_com, p_cscv_id, p_cdcv_id, p_scv, p_dcv, dvdtheta1_s, dvdtheta1_d, power_ind, power_ind_ideal, power_loss_suc, power_loss_disc, power_comp_only)
    
    include 'var_operational_parameter.f90'
    include 'var_operating_fluid_condition.f90'
    include 'var_simulation_parameter.f90'
    double precision power_ind, power_ind_ideal, power_loss_suc, power_loss_disc, power_comp_only

    !real P_ind_id_1, P_ind_id_2, P_ind_1, P_ind_2, P_loss_suc_1, P_loss_suc_2, P_loss_disc_1, P_loss_disc_2
    double precision, dimension (1:no_data+1) :: w_suc_id, w_disc_id, w_disc_all_id, w_suc, w_disc, w_disc_diff
    double precision, dimension (1:no_data+1) :: p_scv, p_dcv, v_suc, v_com
    double precision, dimension (1:no_data+1) :: dvdtheta1_s, dvdtheta1_d
    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id
    integer i
    
    ! ------------------------------------------------- 
    ! Calculate Indicated Work
    ! W_ind = Pressure * dV/d@ * omega_1
    ! or W_ind = Pressure*(change in Volume)*frequency
    ! ------------------------------------------------- 
    
    do 288 i = 1, no_data+1
        
        ! ----------------- first version -------------- !
        !w_suc_id_1(i) = p_cscv_id_1(i)*dvdtheta1_s_1(i)*1000.0*omega_1
        !w_suc_id_2(i) = p_cscv_id_2(i)*dvdtheta1_s_2(i)*1000.0*omega_1
        !w_suc_1(i) = p_scv_1(i)*dvdtheta1_s_1(i)*1000.0*omega_1            ! work done during suction process
        !w_suc_2(i) = p_scv_2(i)*dvdtheta1_s_2(i)*1000.0*omega_1           
        !
        !w_disc_1(i) = - p_dcv_1(i)*dvdtheta1_d_1(i)*1000.0*omega_1         ! work done in discharge process (volume decreasing so using -ve)
        !w_disc_2(i) = - p_dcv_2(i)*dvdtheta1_d_2(i)*1000.0*omega_1
        !w_disc_all_id_1(i) = - p_cdcv_id_1(i)*dvdtheta1_d_1(i)*1000.0*omega_1
        !w_disc_all_id_2(i) = - p_cdcv_id_2(i)*dvdtheta1_d_2(i)*1000.0*omega_1
        
        ! ---------------- Second version -------------- !
        !w_suc_id_1(i) = p_cscv_id_1(i)*(v_suc1(i+1)-v_suc1(i))*1000.0*1.d-9*freq
        !w_suc_id_2(i) = p_cscv_id_2(i)*(v_suc2(i+1)-v_suc2(i))*1000.0*1.d-9*freq
        !w_suc_1(i) = p_scv_1(i)*(v_suc1(i+1)-v_suc1(i))*1000.0*1.d-9*freq            ! work done during suction process
        !w_suc_2(i) = p_scv_2(i)*(v_suc2(i+1)-v_suc2(i))*1000.0*1.d-9*freq           
        !
        !w_disc_1(i) = - p_dcv_1(i)*(v_com1(i+1)-v_com1(i))*1000.0*1.d-9*freq         ! work done in discharge process (volume decreasing so using -ve)
        !w_disc_2(i) = - p_dcv_2(i)*(v_com2(i+1)-v_com2(i))*1000.0*1.d-9*freq
        !w_disc_all_id_1(i) = - p_cdcv_id_1(i)*(v_com1(i+1)-v_com1(i))*1000.0*1.d-9*freq
        !w_disc_all_id_2(i) = - p_cdcv_id_2(i)*(v_com2(i+1)-v_com2(i))*1000.0*1.d-9*freq
        ! -------------------------
        ! Use middle point
        ! -------------------------
        !w_suc_id(i) = 0.5*(p_cscv_id(i) + p_cscv_id(i+1))*dvdtheta1_s(i)*1000.0*omega_1
        !w_suc(i) = 0.5*(p_scv(i) + p_scv(i+1))*dvdtheta1_s(i)*1000.0*omega_1            ! work done during suction process
        !    
        !w_disc(i) = - 0.5*(p_dcv(i) + p_dcv(i+1))*dvdtheta1_d(i)*1000.0*omega_1         ! work done in discharge process (volume decreasing so using -ve)
        !w_disc_all_id(i) = - 0.5*(p_cdcv_id(i) + p_cdcv_id(i+1))*dvdtheta1_d(i)*1000.0*omega_1
        ! ------------------
        ! use the exact point
        ! ------------------
        w_suc_id(i) = p_cscv_id(i)*dvdtheta1_s(i)*1000.0*omega_1
        w_suc(i) = p_scv(i)*dvdtheta1_s(i)*1000.0*omega_1            ! work done during suction process [W]
            
        w_disc(i) = - p_dcv(i)*dvdtheta1_d(i)*1000.0*omega_1         ! work done in compression process (volume decreasing so using -ve) [W]
        w_disc_all_id(i) = - p_cdcv_id(i)*dvdtheta1_d(i)*1000.0*omega_1
        
        if (p_dcv(i) < p_disc) then
            w_disc_diff(i) = 0.0
            w_disc_id(i) = 0.0
        else
            !w_disc_diff_1(i) = - p_dcv_1(i)*(v_com1(i+1)-v_com1(i))*1000.0*1.d-9*freq  
            !w_disc_id_1(i) = -p_cdcv_id_1(i)*(v_com1(i+1)-v_com1(i))*1000.0*1.d-9*freq
            ! ----------------------
            ! Use mid point
            ! ----------------------
            !w_disc_diff(i) = -0.5*(p_dcv(i) + p_dcv(i+1))*dvdtheta1_d(i)*1000.0*omega_1 
            !w_disc_id(i) = -0.5*(p_cdcv_id(i) + p_cdcv_id(i+1))*dvdtheta1_d(i)*1000.0*omega_1
            ! --------------------
            ! Use the exact point
            ! --------------------
            w_disc_diff(i) = -p_dcv(i)*dvdtheta1_d(i)*1000.0*omega_1 
            w_disc_id(i) = -p_cdcv_id(i)*dvdtheta1_d(i)*1000.0*omega_1
        endif
        
        !if (p_dcv(i) < p_disc) then
            !w_comp(i) = - 0.5*(p_dcv(i) + p_dcv(i+1))*dvdtheta1_d(i)*1000.0*omega_1
            !w_comp_id(i) = - 0.5*(p_cdcv_id(i) + p_cdcv_id(i+1))*dvdtheta1_d(i)*1000.0*omega_1
        !else
        !    w_comp(i) = 0.0
        !    w_comp_id(i) = 0.0
        !endif
288 continue 
        
        power_ind_ideal = (- sum(w_suc_id) + sum(w_disc_all_id))/(no_data+1)
        power_ind = (- sum(w_suc) + sum(w_disc))/(no_data+1)
        power_loss_suc = (sum(w_suc_id) - sum(w_suc))/(no_data+1)
        power_loss_disc = (sum(w_disc_diff) - sum(w_disc_id))/(no_data+1)
        power_comp_only = (sum(w_disc) - sum(w_disc_all_id))/(no_data+1)

        write (33, 2981) 'P_ind_id[W]','P_ind[W]','Suct.Loss[W]','Disc.Loss[W]','P.Comp.loss[W]'
        write (33, 2983) power_ind_ideal, power_ind, power_loss_suc, power_loss_disc, power_comp_only
        write (33,*) ' ----------------------------------------------------- '

        ! -----------------------------------------------
        !  Write into overview file
        ! -----------------------------------------------
        !write (113,*) " -------------------------------------------------------------------- "
        !write (113,*) " Compressor Performance "
        !write (113,*) " -------------------------------------------------------------------- "
        !write (113,2990) "Total Volume                          vol_total           = ", vol_total/1000., " cc or cm3" 
        !write (113,2990) "Total Mass Flow Rate                  mass_total          = ", mass_total, " kg/s"
        !! Cooling capacity
        !!write (113,2989) "Cooling Capacity                      q_capacity          = ", q_capacity/1000., " kW"
        !!write (113,2989) "Indicated Work for Outer Compressor   P_ind_1             = ", P_ind_1/1000., " kW"
        !!write (113,2989) "Indicated Work for Inner Compressor   P_ind_2             = ", P_ind_2/1000., " kW"
        !!write (113,2989) "Total Indicated Work for MCRC         P_ind               = ", P_ind/1000., " kW"
        !write (113,2990) "Cooling Capacity                      q_capacity          = ", q_capacity, " W"
        !write (113,2990) "Indicated Work for Outer Compressor   P_ind_1             = ", P_ind_1, " W"
        !write (113,2990) "Indicated Work for Inner Compressor   P_ind_2             = ", P_ind_2, " W"
        !write (113,2990) "Total Indicated Work for MCRC         P_ind               = ", P_ind, " W"
        !
        !! ---- Ideal Overview ---- !
        !write(45,2990) "Id. Indicated Work for Outer Compressor   P_ind_id_1             = ", P_ind_id_1, " W"
        !write(45,2990) "Id. Indicated Work for Inner Compressor   P_ind_id_2             = ", P_ind_id_2, " W"
        !write(45,2990) "Id. Indicated Work for Outer Compressor   P_ind_id               = ", P_ind_id, " W"
        
        
2981 format (14A25)
2982 format (F25.4, 14ES25.6)
2983 format (14ES25.6)  
2989 format (2x,A,F8.4,A)
2990 format (2x,A,f15.4,A)
endsubroutine indicated_work