! include "var_leakage.f90"
    double precision, dimension (1:no_data+1) :: dmdtheta1_leak_s, dedtheta1_leak_s, dmdtheta1_leak_d, dedtheta1_leak_d
    double precision, dimension (1:no_data+1) :: dmdtheta1_leak_vs, dmdtheta1_leak_vef, dmdtheta1_leak_ref, dmdtheta1_leak_rad_ro, dmdtheta1_leak_rad_vh
    double precision avrg_leakage_mass, avrg_leak_vs, avrg_leak_vef, avrg_leak_ref, avrg_leak_rad_ro, avrg_leak_rad_vh
    common/leakage_result_para/avrg_leakage_mass, avrg_leak_vs, avrg_leak_vef, avrg_leak_ref, avrg_leak_rad_ro, avrg_leak_rad_vh