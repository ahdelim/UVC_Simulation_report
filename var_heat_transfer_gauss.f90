    integer, parameter :: G_E = 19 !G_E = 9 if it is 9 equations 9 unknowns
    double precision U_ht_element(G_E,G_E), C_RHS(G_E), sol_T(G_E)
    
     ! --- Results output --- !
    double precision, dimension (1:no_data+1) :: T_ocsc, T_ocdc, T_rosc, T_rodc, T_roller, T_uocsc, T_locsc, T_uocdc, T_locdc
    double precision, dimension (1:no_data+1) :: T_ocsc2, T_ocsc3, T_ocdc2, T_ocdc3, T_rosc2, T_rosc3, T_rodc2, T_rodc3, T_roller2, T_roller3
    
    ! --- Various radius and area
    double precision r_ococo, A_ocscdc, r_roiro, A_roscdc, l_net, A_cover_cross, A_base_cross
    double precision l_ocsc, l_ocdc, l_rosc, l_rodc, l_uocsc, l_uocdc, l_locsc, l_locdc
    double precision A_ocsc_cross, A_ocdc_cross, A_rosc_cross, A_rodc_cross, A_scroller_cross, A_dcroller_cross
    
    ! --- For Element 1 -- ocsc
    double precision U_ocsc_ocsc, U_ocsc_sc, U_ocsc_hc, U_ocsc_ocdc, U_ocsc_uocsc, U_ocsc_ocsc2
    double precision H_ocsc_sc, H_ocsc_hc, H_ocsc_ocdc, H_ocsc_uocsc, H_ocsc_ocsc2, C_ocsc
    
    ! --- For Element 1.2 -- ocsc2
    double precision U_ocsc2_ocsc2, U_ocsc2_sc, U_ocsc2_hc, U_ocsc2_ocdc2, U_ocsc2_ocsc, U_ocsc2_ocsc3
    double precision H_ocsc2_sc, H_ocsc2_hc, H_ocsc2_ocdc2, H_ocsc2_ocsc, H_ocsc2_ocsc3, C_ocsc2
    
    ! --- For Element 1.3 -- ocsc3
    double precision U_ocsc3_ocsc3, U_ocsc3_sc, U_ocsc3_hc, U_ocsc3_ocdc3, U_ocsc3_ocsc2, U_ocsc3_locsc
    double precision H_ocsc3_sc, H_ocsc3_hc, H_ocsc3_ocdc3, H_ocsc3_ocsc2, H_ocsc3_locsc, C_ocsc3
        
    ! --- For Element 2 -- ocdc
    double precision U_ocdc_ocdc, U_ocdc_dc, U_ocdc_hc, U_ocdc_ocsc, U_ocdc_uocdc, U_ocdc_ocdc2
    double precision H_ocdc_dc, H_ocdc_hc, H_ocdc_ocsc, H_ocdc_uocdc, H_ocdc_ocdc2, C_ocdc
    
    ! --- For Element 2.2 -- ocdc2
    double precision U_ocdc2_ocdc2, U_ocdc2_dc, U_ocdc2_hc, U_ocdc2_ocsc2, U_ocdc2_ocdc, U_ocdc2_ocdc3
    double precision H_ocdc2_dc, H_ocdc2_hc, H_ocdc2_ocsc2, H_ocdc2_ocdc, H_ocdc2_ocdc3, C_ocdc2
    
    ! --- For Element 2.3 -- ocdc3
    double precision U_ocdc3_ocdc3, U_ocdc3_dc, U_ocdc3_hc, U_ocdc3_ocsc3, U_ocdc3_ocdc2, U_ocdc3_locdc
    double precision H_ocdc3_dc, H_ocdc3_hc, H_ocdc3_ocsc3, H_ocdc3_ocdc2, H_ocdc3_locdc, C_ocdc3
    
    ! --- For Element 3 -- rosc
    double precision U_rosc_rosc, U_rosc_sc, U_rosc_roller, U_rosc_rodc, U_rosc_uocsc, U_rosc_rosc2
    double precision H_rosc_sc, H_rosc_roller, H_rosc_rodc, H_rosc_uocsc, H_rosc_rosc2, C_rosc
    
    ! --- For Element 3.2 -- rosc2
    double precision U_rosc2_rosc2, U_rosc2_sc, U_rosc2_roller2, U_rosc2_rodc2, U_rosc2_rosc, U_rosc2_rosc3
    double precision H_rosc2_sc, H_rosc2_roller2, H_rosc2_rodc2, H_rosc2_rosc, H_rosc2_rosc3, C_rosc2
    
    ! --- For Element 3.3 -- rosc3
    double precision U_rosc3_rosc3, U_rosc3_sc, U_rosc3_roller3, U_rosc3_rodc3, U_rosc3_rosc2, U_rosc3_locsc
    double precision H_rosc3_sc, H_rosc3_roller3, H_rosc3_rodc3, H_rosc3_rosc2, H_rosc3_locsc, C_rosc3
    
    ! --- For Element 4 -- rodc
    double precision U_rodc_rodc, U_rodc_dc, U_rodc_roller, U_rodc_rosc, U_rodc_uocdc, U_rodc_rodc2
    double precision H_rodc_dc, H_rodc_roller, H_rodc_rosc, H_rodc_uocdc, H_rodc_rodc2, C_rodc
    
    ! --- For Element 4.2 -- rodc2
    double precision U_rodc2_rodc2, U_rodc2_dc, U_rodc2_roller2, U_rodc2_rosc2, U_rodc2_rodc, U_rodc2_rodc3
    double precision H_rodc2_dc, H_rodc2_roller2, H_rodc2_rosc2, H_rodc2_rodc, H_rodc2_rodc3, C_rodc2
    
    ! --- For Element 4.3 -- rodc3
    double precision U_rodc3_rodc3, U_rodc3_dc, U_rodc3_roller3, U_rodc3_rosc3, U_rodc3_rodc2, U_rodc3_locdc
    double precision H_rodc3_dc, H_rodc3_roller3, H_rodc3_rosc3, H_rodc3_rodc2, H_rodc3_locdc, C_rodc3
    
    ! --- For Element 5 -- roller
    double precision U_roller_roller, U_roller_rosc, U_roller_rodc, U_roller_uocsc, U_roller_roller2, U_roller_uocdc
    double precision H_roller_rosc, H_roller_rodc, H_roller_uocsc, H_roller_roller2, H_roller_uocdc, C_roller
    
    ! --- For Element 5.2 -- roller2
    double precision U_roller2_roller2, U_roller2_rosc2, U_roller2_rodc2, U_roller2_roller, U_roller2_roller3
    double precision H_roller2_rosc2, H_roller2_rodc2, H_roller2_roller, H_roller2_roller3, C_roller2
    
    ! --- For Element 5.3 -- roller3
    double precision U_roller3_roller3, U_roller3_rosc3, U_roller3_rodc3, U_roller3_roller2, U_roller3_locsc, U_roller3_locdc
    double precision H_roller3_rosc3, H_roller3_rodc3, H_roller3_roller2, H_roller3_locsc, H_roller3_locdc, C_roller3
    
    ! --- For Element 6 -- uocsc
    double precision U_uocsc_uocsc, U_uocsc_sc, U_uocsc_hc, U_uocsc_rosc, U_uocsc_ocsc, U_uocsc_roller, U_uocsc_uocdc
    double precision H_uocsc_sc, H_uocsc_hc, H_uocsc_rosc, H_uocsc_ocsc, H_uocsc_roller, H_uocsc_uocdc, C_uocsc
    
    ! --- For Element 7 -- locsc
    double precision U_locsc_locsc, U_locsc_sc, U_locsc_resoil, U_locsc_rosc3, U_locsc_ocsc3, U_locsc_roller3, U_locsc_locdc
    double precision H_locsc_sc, H_locsc_resoil, H_locsc_rosc3, H_locsc_ocsc3, H_locsc_roller3, H_locsc_locdc, C_locsc
    
    ! --- For Element 8 -- uocdc
    double precision U_uocdc_uocdc, U_uocdc_dc, U_uocdc_hc, U_uocdc_rodc, U_uocdc_ocdc, U_uocdc_roller, U_uocdc_uocsc
    double precision H_uocdc_dc, H_uocdc_hc, H_uocdc_rodc, H_uocdc_ocdc, H_uocdc_roller, H_uocdc_uocsc, C_uocdc
    
    ! --- For Element 9 -- locdc
    double precision U_locdc_locdc, U_locdc_dc, U_locdc_resoil, U_locdc_rodc3, U_locdc_ocdc3, U_locdc_roller3, U_locdc_locsc
    double precision H_locdc_dc, H_locdc_resoil, H_locdc_rodc3, H_locdc_ocdc3, H_locdc_roller3, H_locdc_locsc, C_locdc
    
    
    ! include "var_heat_transfer_gauss.f90"