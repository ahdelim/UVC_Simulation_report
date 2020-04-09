subroutine kine_vane(dlvdt, dlvdtt, dlvdtheta1, dlvdtheta12, theta_1)
    
    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    include "var_operational_parameter.f90"
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"
    include "var_kinematics.f90"
    integer i
    
    
    do 450 i = 1, no_data + 1
        ! --------------------
        ! dl/dtheta1, d2l/dtheta1, dldt, and dldtt
        ! --------------------
        
        dlvdtheta1(i) = (e*(r_oc + r_vh)*sin(theta_1(i)))/sqrt(e**2 + (r_oc + r_vh)**2 - 2*e*(r_oc + r_vh)*cos(theta_1(i)))     ![mm/rad]
        dlvdtheta12(i) = e*(r_oc + r_vh)*(cos(theta_1(i))/sqrt(e**2 + (r_oc + r_vh)**2 - 2*e*(r_oc + r_vh)*cos(theta_1(i))) - e*(r_oc + r_vh)*sin(theta_1(i))**2 / ((e**2 + (r_oc + r_vh)**2 - 2*e*(r_oc + r_vh)*cos(theta_1(i)))**1.5))
        dlvdt(i)  = omega_1*dlvdtheta1(i)                                   ! Vane sliding velocity
        dlvdtt(i) = alpha_1*dlvdtheta1(i) + omega_1**2.0*dlvdtheta12(i)       ! Vane sliding acceleration
450 continue
        !dlvdtheta1(1) = dlvdtheta1(no_data+1)
    
end subroutine kine_vane