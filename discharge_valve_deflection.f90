subroutine discharge_valve_deflection(no_data, f_start, f_mid, f_end, integral_f, y_stop, d_port, A_cross_dv, rho_dv, omega_response_dv, dpratio_dv, p_dcv, p_disc, g_r, h_r, ystartn, yendn, gn, hn)
    
    include "var_physical_constants.f90"
    double precision f_start, f_mid, f_end, integral_f, theta_next, theta_now, omega_1, y_stop, d_port, A_cross_dv, rho_dv, omega_response_dv, dpratio_dv, p_dcv, p_disc, g_r, h_r
    double precision area_port, time_step
    double precision gn, hn, ystartn, yendn
    integer no_data
    
    !time_step = (theta_next - theta_now)/omega_1
    area_port = 0.25*pi*(d_port/1000.0)*(d_port/1000.0)     ! A_hole
    
    call Runge_valve(g_r, h_r, f_mid, integral_f, p_dcv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, g_grav, omega_response_dv, gn, hn)
    !call RK4_gnhn(g_r, h_r, time_step, f_mid, p_dcv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, omega_response_dv, gn, hn)
    
    ystartn = f_start*gn
    yendn = f_end*gn
    
    if (ystartn < 0.0) then
        ystartn = 0.0
        gn = 0.0    ! amplitude
        hn = 0.0    ! valve velocity
    elseif (ystartn == 0.0 .and. hn < 0.0) then
        ystartn = 0.0
        gn = 0.0
        hn = 0.0
    elseif (ystartn > y_stop*1.d-3) then
        ystartn = y_stop*1.d-3
        gn = y_stop*1.d-3/f_start
        hn = 0.0
    elseif (ystartn == y_stop*1.d-3 .and. hn > 0.0) then
        ystartn = y_stop*1.d-3
        gn = y_stop*1.d-3/f_start
        hn = 0.0
    endif
    
    if (yendn < 0.0) then
        yendn = 0.0
        gn = 0.0
        hn = 0.0
    elseif (yendn == 0.0 .and. hn < 0.0) then
        yendn = 0.0
        gn = 0.0
        hn = 0.0
    elseif (yendn > y_stop*1.d-3) then
        yendn = y_stop*1.d-3
        gn = y_stop*1.d-3/f_end
        hn = 0.0
    elseif (yendn == y_stop*1.d-3 .and. hn > 0.0) then
        yendn = y_stop*1.d-3
        gn = y_stop*1.d-3/f_end
        hn = 0.0
    endif

endsubroutine