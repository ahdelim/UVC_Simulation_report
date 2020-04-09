subroutine geo_m(theta_1, theta_2, l_v, v_com, v_suc, gamma_1)
    
    include "var_simulation_parameter.f90"
    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    
    include "var_compressor_evaluation.f90"
    integer optimization_opt
    common/routine_opt/optimization_opt
    
    ! Define theta_1
    call geo_revolution_theta_1(theta_1)
    
    ! Define varying vane length
    call geo_vanelength(l_v, theta_1, r_oc, r_ro, r_vh, e)
    
    ! Define theta_2 & gamma_1
    call geo_revolution_theta_2(theta_2, theta_1, l_v, r_oc, r_ro, r_vh, e)
    call geo_comp_gamma_1(gamma_1, l_v, theta_1, r_oc, r_vh, r_ro, e)
    
    ! Define chamber volume changes
    call geo_comp_volume(v_suc, v_com, theta_1, theta_2, l_v, gamma_1, l_com, r_oc, r_ro, r_vh, l_vh, e, w_v_ro,  w_vs_ro, l_vs_ro, w_vsro_gap, l_vsro_gap, dia_suc, dia_disc, length_suc, length_disc, w_vs_oil_path, h_vs_oil_path, l_vs_oil_path)
    
    ! Total volume
    call geo_total_vol

    ! ------------------------------
    ! Write data into files
    ! if optimization is on, don't write
    ! ------------------------------
    write(8,2981) "Degree", "Suct. Vol[m3]", "Comp. Vol[m3]"
    write(12,2981) "Degree", "Length_v[mm]"
    
    do 401 i = 1, no_data+data_step, data_step
        write (3,2982) theta_1(i)*180.0/pi, theta_2(i)*180.0/pi
        write (8,2982), theta_1(i)*180.0/pi, v_suc(i)*1.d-9 ,v_com(i)*1.d-9        ! in m^3
        write (12,2982) theta_1(i)*180.0/pi, l_v(i)    ! [mm]
401 continue  
    
2981 format (11A25)
2982 format (F25.4, 14ES25.6) 
end subroutine geo_m