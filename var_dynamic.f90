
    ! --------------------------------
    ! for 9 unknown cases (tested and failed on 21-11-2017)
    ! --------------------------------
    !double precision, dimension (1:no_data+1) :: F_ox, F_oy, T_vhro, M_o, F_1_n, F_2_n, F_cx, F_cy, T_roll       ! Forces to be determined (for 9 unknowns and 9 equations)
    ! ---------------------------------
    ! for 6 unknowns case
    ! ---------------------------------
    double precision, dimension (1:no_data+1) :: F_vhoc_n, F_vhoc_t, F_1_n, F_2_n, F_rx, F_ry, F_cx, F_cy, F_resultant       ! Forces to be determined (for 6 unknowns)
    !double precision, dimension (1:no_data+1) :: F_1_n, F_2_n, T_vhoc, F_cx, F_cy, T_roll       ! Forces to be determined (for 6 unknowns)
    double precision, dimension (1:no_data+1) :: I_ro_O
    double precision, dimension (1:no_data+1) :: T_ecc, T_inertia_ro, T_inertia_vh, T_com_C, T_com_O, T_total_no_loss, T_inst_total, T_bear_s          ! components of torque
    double precision I_vh_O, T_lub      ! Moment of inertia about O of vane housing and rotor, lubricant frictional torque (between roller and rotor, assume constant first)
    double precision mass_vh, mass_rotor
    double precision T_avrg_input, T_avrg_no_loss, T_avrg_loss, T_avrg_I_ro, T_avrg_I_vh, T_avrg_com, T_avrg_bear_s, T_avrg_ecc, T_avrg_lip_seal, T_peak         ! average torque without any loss
    
    common/moment_of_inertia/mass_vh, mass_rotor, I_vh_O
    common/frictional_torque/T_lub
    common/dynamic_torque/T_avrg_input, T_avrg_no_loss, T_avrg_loss, T_avrg_I_ro, T_avrg_I_vh, T_avrg_com, T_avrg_bear_s, T_avrg_ecc, T_avrg_lip_seal, T_peak 
    ! include "var_dynamic.f90"