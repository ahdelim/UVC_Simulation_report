subroutine kinematics_m(theta_1, gamma_1, l_v, dlvdt, dlvdtt, dgammadt_1, dgammadtt_1, dvdtheta1_suc, dvdtheta1_com)
    
    ! Simulation/operational parameters and constants
    include "var_operational_parameter.f90"
    include "var_simulation_parameter.f90"
    include "var_physical_constants.f90"
    
    ! Geometrical model and main dimensions
    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    
    ! Kinematics and Differentiated Geometrical variables
    include "var_kinematics.f90"
    include "var_geometrical_diff.f90"
    integer optimization_opt
    common/routine_opt/optimization_opt
    
    ! Vane Velocity and Acceleration
    call kine_vane(dlvdt, dlvdtt, dlvdtheta1, dlvdtheta12, theta_1)
    
    ! Rotor swinging movement
    call kine_rotor(theta_1, gamma_1, l_v, dlvdt, dlvdtt, dgammadt_1, dgammadtt_1, dlvdtheta1, dlvdtheta12, dtheta2dt)
    
    ! Working chambers rate of change
    call kine_working_chamber(theta_1, dlvdtheta1, dtheta2dt, dvdtheta1_suc, dvdtheta1_com)
    
    ! ------------------------------
    ! Write data into files (when no optimization)
    ! ------------------------------
    ! --- Create file headers
    write(11, 2985) "Degree", "dlvdt[mm/s]", "dlvdtt[mm/s2]"
    write(22, 2985) "Degree", "gamma_1[rad]","dgammadt_1[rad/s]", "dgammadtt_1[rad/s2]"
    write(61, 2985) "Degree", "dvdtheta1_suc[mm3/rad]", "dvdtheta1_com[mm3/rad]"
    ! --- Data writing
    do 451 i = 1, no_data+data_step, data_step
        write (11,2986) theta_1(i)*180./pi, dlvdt(i), dlvdtt(i)      ! [mm/s] [mm/s2]
        write (22,2986) theta_1(i)*180./pi, gamma_1(i), dgammadt_1(i), dgammadtt_1(i)     ! [rad/s]
        write (61,2986) theta_1(i)*180./pi, dvdtheta1_suc(i)*1.0d-9, dvdtheta1_com(i)*1.0d-9 ![m3]
451 continue     
    
2985 format (11A25)
2986 format (F25.4, 10ES25.6) 

endsubroutine kinematics_m
    
    
!        call smoothing_data(no_data, dvdtheta1_suc1)
!        call smoothing_data(no_data, dvdtheta1_com1)
!        call smoothing_data(no_data, dvdtheta1_suc2)
!        call smoothing_data(no_data, dvdtheta1_com2)