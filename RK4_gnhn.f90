subroutine RK4_gnhn(g_r, h_r, time_step, f_mid, p_cv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, omega_response_dv, gn, hn)
    
    real g_r, h_r, time_step, f_mid, p_cv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, omega_response_dv, gn, hn
    real dg, dh, g, h
    integer i
    gn = g_r
    hn = h_r
    dg = 0
    dh = 0
    
    do 278 i = 1,4
        
        if (i == 1) then
            g = g_r
            h = h_r
        elseif (i == 2 .or. i == 3) then
            g = g_r + 0.5*dg
            h = h_r + 0.5*dh
        elseif (i == 4) then
            g = g_r + dg
            h = h_r + dh
        endif
        
        dg = time_step*h
        dh = time_step*(f_mid*(p_cv - p_disc)*1000*area_port/(rho_dv*A_cross_dv) - 2*dpratio_dv*omega_response_dv*h - (omega_response_dv)*(omega_response_dv)*g)

        if ( i == 1 .or. i == 4) then
            n = 0.166666666
        elseif ( i == 2 .or. i == 3) then
            n = 0.333333333
        endif
    
        gn = gn + n*dg
        hn = hn + n*dh
    
278 continue    
endsubroutine RK4_gnhn