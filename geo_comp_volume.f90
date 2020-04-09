subroutine geo_comp_volume(v_suc, v_com, theta_1, theta_2, l_v, gamma_1, l_com, r_oc, r_ro, r_vh, l_vh, e, w_v_ro, w_vs_ro, l_vs_ro, w_vsro_gap, l_vsro_gap, dia_suc, dia_disc, length_suc, length_disc, w_vs_oil_path, h_vs_oil_path, l_vs_oil_path)
    implicit none
    include "var_simulation_parameter.f90"
    include "var_physical_constants.f90"
    include "var_clearance.f90"
    
    double precision, dimension (1:no_data+1) :: v_suc, v_com, theta_1, theta_2, l_v, gamma_1
    double precision l_com, r_oc, r_ro, r_vh, l_vh, e, w_v_ro,  w_vs_ro, l_vs_ro
    double precision w_vsro_gap, l_vsro_gap, w_vs_oil_path, h_vs_oil_path, l_vs_oil_path
    double precision dia_suc, dia_disc, length_suc, length_disc
    integer i
    
    
    ! Clearance volume
    v_clearance_s = w_vsro_gap*l_vsro_gap*l_com !+ 4.*w_vs_oil_path**2*pi*h_vs_oil_path !+ pi*(0.5*dia_disc)**2*length_disc      !w_vs_oil_path*h_vs_oil_path*l_vs_oil_path    !500.0 !0.25*pi*length_suc*dia_suc**2  ! in [mm3]  ! ** 93.8 is the gap between vane housing and cylinder 
    v_clearance_d = w_vsro_gap*l_vsro_gap*l_com !+ 4.*w_vs_oil_path**2*pi*h_vs_oil_path !+ pi*(0.5*dia_disc)**2*length_disc       !w_vs_oil_path*h_vs_oil_path*l_vs_oil_path ! 400.0 !0.25*pi*length_disc*dia_disc**2  ! in [mm3]

    
    do 400 i = 1, no_data+1
        ! --------------------
        ! volume unit in mm3
        ! v_suc is the suction volume
        ! v_com is the discharge/compression volume
        ! --------------------
        !if (abs(l_v(i)) <= l_vs_ro) then
            v_suc(i) = v_clearance_s + 0.5*l_com*(theta_1(i)*r_oc**2 - theta_2(i)*r_ro**2 - e*(r_oc + r_vh)*sin(theta_1(i)) - w_v_ro*l_v(i) + (-gamma_1(i))*r_vh**2) + 0.125*w_v_ro**2*tan(-gamma_1(i))*l_com ! + l_v(i)*w_vs_ro*l_com     ! 3.45 is the average of the tapered volume
        !else
            
        !    v_suc(i) = v_clearance_s + 0.5*l_com*(theta_1(i)*r_oc**2 - theta_2(i)*r_ro**2 - e*(r_oc + r_vh)*sin(theta_1(i)) - w_v_ro*l_v(i) + (-gamma_1(i))*r_vh**2) + 0.125*w_v_ro**2*tan(-gamma_1(i))*l_com   ! + l_vs_ro*w_vs_ro*l_com - (l_v(i) - l_vh)*w_vs_ro*l_com    (for version 2-2, vane slot volume is not added because vane housing vane occupies almost the same volume)
        !endif
        ! -----------------
        ! original
        ! --------------
        !v_suc(i) = v_clearance_s + 0.5*l_com*(theta_1(i)*r_oc**2 - theta_2(i)*r_ro**2 - e*(r_oc + r_vh)*sin(theta_1(i)) - w_v_ro*l_v(i) + (-gamma_1(i))*r_vh**2) + 0.125*w_v_ro**2*tan(-gamma_1(i))*l_com 
        
        !!v_com(i) = v_clearance_d + pi*l_com*(r_oc**2 - r_ro**2) - 0.5*l_com*(theta_1(i)*r_oc**2 - theta_2(i)*r_ro**2 - e*(r_oc + r_vh)*sin(theta_1(i)) + w_v_ro*l_v(i) - tan(gamma_1(i))*r_vh**2) + 0.125*w_v_ro**2*tan(gamma_1(i))*l_com
        !if (abs(l_v(i)) <= l_vs_ro) then
            v_com(i) = v_clearance_s + v_clearance_d + pi*l_com*(r_oc**2 - r_ro**2) - v_suc(i) - l_com*w_v_ro*l_v(i) !+ l_v(i)*l_com*3.45 ! version 2-2 
        !else
        !    v_com(i) = v_clearance_s + v_clearance_d + pi*l_com*(r_oc**2 - r_ro**2) - v_suc(i) - l_com*w_v_ro*l_v(i) ! + l_vs_ro*l_com*3.45 - (l_v(i) - l_vh)*w_vs_ro*l_com
        !endif
        ! -----------------
        ! original
        ! --------------
        !v_com(i) = v_clearance_s + v_clearance_d + pi*l_com*(r_oc**2 - r_ro**2) - v_suc(i) - l_com*w_v_ro*l_v(i)
        
        ! gamma_1(i)*r_vh**2 is the vane head volume, and the last term accounts for the vane width when inclined
        ! ----------------------------
        ! to check volume if correct
        ! write onto test writing file (no.99)
        ! ----------------------------
        !write (99,*) v_suc(i), v_com(i)
400 continue
        

end subroutine geo_comp_volume