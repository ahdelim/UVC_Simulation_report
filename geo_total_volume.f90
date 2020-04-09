subroutine geo_total_vol
    
    include "var_compressor_evaluation.f90"
    include "var_physical_constants.f90"
    include "var_main_dimensions.f90"
    include "var_clearance.f90"
    
    double precision vol_compressor
    ! -----------------------------
    ! total volume of the compressor
    ! -----------------------------
    vol_compressor = pi*(r_oc**2 - r_ro**2)*l_com + v_clearance_s
    vol_total = vol_compressor
    
    
endsubroutine geo_total_vol