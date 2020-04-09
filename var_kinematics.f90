
    double precision, dimension (1:no_data+1) :: dgammadtheta1_1
    double precision, dimension (1:no_data+1) :: dgammadt_1, dgammadtt_1
    double precision, dimension (1:no_data+1) :: dlvdtheta1, dlvdtheta12
    double precision, dimension (1:no_data+1) :: dtheta2dt
    double precision, dimension (1:no_data+1) :: dlvdt, dlvdtt                  ! Velocity and Acceleration of vane
    !double precision, dimension (1:no_data+1) :: omega_2, omega_3, alpha_2          ! hollow cylinder (angular)

    
    
    ! include "var_kinematics.f90"