double precision r_hc, r_oc, r_oco, t_oc, r_ro, r_roi, r_vh, r_shaft, l_vh, l_v_ro, l_exposed_vh, l_v_max, l_vs_ro, w_vs_ro, w_v_ro, w_v_max, l_com, t_ro, e, r_ecc, l_ecc    ! Main design dimensions
double precision t_oc_cover, t_oc_base      ! sub dimensions
double precision w_vsro_gap, l_vsro_gap, w_vs_oil_path, h_vs_oil_path, l_vs_oil_path     ! gap/clearance dimensions between the vane housing vane and the rotor vane slot 
double precision r_bearing, l_bearing, l_bearing_lower
double precision dia_suc, dia_disc, length_suc, length_disc                 ! Ports Diameters & Port Length
double precision theta_suc_center, theta_suc_start, theta_suc_end, theta_disc_center, theta_disc_start, theta_disc_end       ! Port position parameter
double precision dia_dv, w_dv, t_dv, l_dv, x_hole_start, y_stop                     ! Outer Valve dimensions
double precision l_dv_ef, w_dv_x2, r_dv_x1, l_dv_x2, l_dv_x3, l_dv_neck                            ! Valve length and width in fraction
double precision w_dv_rec, l_dv_rec, l_dv_rec_ef

common/main_dimension_size/r_hc, r_oc, r_oco, t_oc, r_ro, r_roi, r_vh, r_shaft, l_vh, l_v_ro, l_exposed_vh, l_v_max, l_vs_ro, w_vs_ro, w_v_ro, w_v_max, l_com, t_ro, e, r_ecc, l_ecc       ! common block for main dimension
common/sub_dimension_size/t_oc_cover, t_oc_base
common/other_dimension/w_vsro_gap, l_vsro_gap, w_vs_oil_path, h_vs_oil_path, l_vs_oil_path   
common/bearing_dimension/r_bearing, l_bearing, l_bearing_lower
common/port_size_var/dia_suc, dia_disc, length_suc, length_disc      ! common block for PORT
common/port_position_par/theta_suc_center, theta_suc_start, theta_suc_end, theta_disc_center, theta_disc_start, theta_disc_end   
common/discharge_valve_dimension/dia_dv, w_dv, t_dv, l_dv, x_hole_start, y_stop, l_dv_ef, w_dv_x2, r_dv_x1, l_dv_x2, l_dv_x3, l_dv_neck 
common/rec_discharge_valve/w_dv_rec, l_dv_rec, l_dv_rec_ef

!include "var_main_dimensions.f90"