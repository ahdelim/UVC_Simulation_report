subroutine kine_working_chamber(theta_1, dlvdtheta1, dtheta2dt, dvdtheta1_suc, dvdtheta1_com)
    
    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    include "var_operational_parameter.f90"
    include "var_simulation_parameter.f90"
    include "var_kinematics.f90"
    include "var_geometrical_diff.f90"
    integer i
    
    ! ---------------------------------
    ! 30-May-2017
    ! The shapes of both outer and inner volume 
    ! change rate are not correct
    ! ---------------------------------
    do 453 i = 1, no_data+1
        
        dvdtheta1_suc(i) = 0.5*( r_oc**2 - r_ro**2*dtheta2dt(i)/omega_1 - e*(r_oc + r_vh)*cos(theta_1(i)) - w_v_ro*dlvdtheta1(i))*l_com
        dvdtheta1_com(i) = 0.5*(-r_oc**2 + r_ro**2*dtheta2dt(i)/omega_1 + e*(r_oc + r_vh)*cos(theta_1(i)) + w_v_ro*dlvdtheta1(i))*l_com
453 continue        

endsubroutine kine_working_chamber