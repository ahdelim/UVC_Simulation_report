subroutine leakage_lambda_mflow(T_d, p_e, k, M, R_specific, miu_gas, clearance, length_flow, lambda_factor, dm_leak_dtheta)
    ! -------------------------------------
    ! leakage_lambda_mflow is a subroutine to find lambda_factor, and leakage mass flow rate of the specific path
    ! lambda_factor is used later for calculation of dummy variables
    ! -------------------------------------
    implicit none
    include "var_operational_parameter.f90"
    double precision T_d, p_e, k, R_specific, clearance       !discharge temperature, mach number, specific heat ratio (k)
    double precision dm_leak_dtheta
    double precision Re, miu_gas, length_flow
    double precision T_e, v_e, lambda_factor
    double precision M
    
    T_e = T_d/(1.0 + M**2*(k - 1.0)/2.0)        ! exit temperature
    v_e = M*sqrt(k*R_specific*T_e)      ! exit velocity in [m/s]
    dm_leak_dtheta = clearance*length_flow*1.d-3*v_e*p_e*1000.0/(T_e*R_specific*omega_1)        ! [kg/rad] leakage mass flow through the radial clearance
    Re = 2.0/(miu_gas*length_flow*1.d-3)*dm_leak_dtheta*omega_1      ! Reynold number
    
    if (Re > 3560.0) then
        lambda_factor = 0.3164/Re**0.25
    else
        lambda_factor = 96.0/Re
    endif

endsubroutine leakage_lambda_mflow
    
    