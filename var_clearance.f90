
    double precision v_clearance_s, v_clearance_d           ! clearance volume
    double precision cl_upend, cl_lowend, cl_vs, cl_rad_vh, cl_rad_ro       ! clearance of the assembly
    double precision cl_bear_s, cl_rad_roll, cl_bear_s_up         ! clearance of bearing components
    double precision cl_eccef_up, cl_eccef_low
    common/var_clear/v_clearance_s, v_clearance_d
    common/clearance_assem/cl_upend, cl_lowend, cl_vs, cl_rad_vh, cl_rad_ro, cl_eccef_up, cl_eccef_low
    common/clearance_bear/cl_bear_s, cl_bear_s_up, cl_rad_roll
    ! include "var_clearance.f90"