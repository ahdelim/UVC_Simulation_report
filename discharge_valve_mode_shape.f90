subroutine discharge_valve_mode_shape(beta_r, x_hole_start, l_dv, d_port, f_start, f_mid, f_end, integral_f)
    
    implicit none
    double precision beta_r, x_hole_start, l_dv, d_port               ! input
    double precision y,h,x,Z,f, A_r                ! variables terms
    double precision f_start, f_mid, f_end, integral_f  ! output
    integer i,n
    
    ! Integration = Weddle's Rule
    ! ------------------- Normalized modes according to orthonormality conditions (Natural Modes) ------------------- !
    y = 0.0
    h = (l_dv*1.d-3)/6.0
    x = 0.0
    
    do 276 i=1,7
        
        Z = ((sin(beta_r*x*1.d-3) - sinh(beta_r*x*1.d-3)) - (sin(beta_r*l_dv*1.d-3) + sinh(beta_r*l_dv*1.d-3))*(cos(beta_r*x*1.d-3)-cosh(beta_r*x*1.d-3))/(cos(beta_r*l_dv*1.d-3) + cosh(beta_r*l_dv*1.d-3)))
        f = Z*Z
        !if (i == 1 .or. i == 7) then
        !    n = 1
        !elseif (i == 3 .or. i == 5) then
        !    n = 2
        !elseif (i == 2 .or. i == 4 .or. i == 6) then
        !    n = 4
        !endif
        if ( i == 1 .or. i == 3 .or. i == 5 .or. i == 7) then
            n = 1
        elseif (i == 2 .or. i == 6) then
            n = 5
        elseif (i == 4) then
            n = 6
        endif
        
        y = y + 0.3*h*n*f
        !y = y + 0.3333333*h*n*f
        x = x + h
        
276 continue        
    
    A_r = sqrt(1.0/y)  ! Find A_r from the integration (magnitude for mode of vibration) ! f_r = valve response profile

    x = x_hole_start*1.d-3
    f_start = A_r*((sin(beta_r*x*1.d-3) - sinh(beta_r*x*1.d-3)) - (sin(beta_r*l_dv*1.d-3) + sinh(beta_r*l_dv*1.d-3))*(cos(beta_r*x*1.d-3)-cosh(beta_r*x*1.d-3))/(cos(beta_r*l_dv*1.d-3) + cosh(beta_r*l_dv*1.d-3)))
    x = (x_hole_start + 0.5*d_port)*1.d-3
    f_mid = A_r*((sin(beta_r*x*1.d-3) - sinh(beta_r*x*1.d-3)) - (sin(beta_r*l_dv*1.d-3) + sinh(beta_r*l_dv*1.d-3))*(cos(beta_r*x*1.d-3)-cosh(beta_r*x*1.d-3))/(cos(beta_r*l_dv*1.d-3) + cosh(beta_r*l_dv*1.d-3)))
    x = (x_hole_start + d_port)*1.d-3
    f_end = A_r*((sin(beta_r*x*1.d-3) - sinh(beta_r*x*1.d-3)) - (sin(beta_r*l_dv*1.d-3) + sinh(beta_r*l_dv*1.d-3))*(cos(beta_r*x*1.d-3)-cosh(beta_r*x*1.d-3))/(cos(beta_r*l_dv*1.d-3) + cosh(beta_r*l_dv*1.d-3)))

    y = 0.0
    h = (l_dv*1.d-3)/6.0
    x = 0.0
    
    do 277 i=1,7
        
        Z = ((sin(beta_r*x*1.d-3) - sinh(beta_r*x*1.d-3)) - (sin(beta_r*l_dv*1.d-3) + sinh(beta_r*l_dv*1.d-3))*(cos(beta_r*x*1.d-3)-cosh(beta_r*x*1.d-3))/(cos(beta_r*l_dv*1.d-3) + cosh(beta_r*l_dv*1.d-3)))
        f = A_r*Z
        !if (i == 1 .or. i == 7) then
        !    n = 1
        !elseif (i == 3 .or. i == 5) then
        !    n = 2
        !elseif (i == 2 .or. i == 4 .or. i == 6) then
        !    n = 4
        !endif
        if ( i == 1 .or. i == 3 .or. i == 5 .or. i == 7) then
            n = 1
        elseif (i == 2 .or. i == 6) then
            n = 5
        elseif (i == 4) then
            n = 6
        endif
    
        y = y + 0.3*h*n*f
        !y = y + 0.3333333*h*n*f
        x = x + h
    
277 continue
    integral_f = y
    
    
   
end subroutine discharge_valve_mode_shape