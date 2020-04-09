subroutine h_cylinder(omega_2, omega_3, alpha_2, dldt_v2)

    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    include "var_operational_parameter.f90"
    include "var_simulation_parameter.f90"
    real termA, termB
    real, dimension (1:no_data+1) :: omega_2, omega_3, alpha_2, dldt_v2
    
    do 460 i = 1, no_data+1  
        ! Hollow cylinder angular velocity w.r.t. contact point of Inner vane with hollow cylinder
        omega_2(i) = omega_1*( 2*(r_ic + l_v2(i))*(r_ic + l_v2(i)) / (r_ih*r_ih + (r_ic + l_v2(i))*(r_ic + l_v2(i)) - (r_ih - r_ic)*(r_ih - r_ic)))
        
        ! Hollow cylinder angular velocity w.r.t. contact point of Outer vane with outer cylinder
        omega_3(i) = omega_1*( (r_oc*r_oc + (r_oh + l_v1(i))*(r_oh + l_v1(i)) - (r_ih - r_ic)*(r_ih - r_ic)) / (2*(r_oh + l_v1(i))*(r_oh + l_v1(i)))) 
        
        ! Hollow cylinder angular accleration
        termA = (2*(r_ic+l_v2(i))*(r_ic+l_v2(i))) / (r_ih*r_ih + (r_ic+l_v2(i))*(r_ic+l_v2(i)) - (r_ih-r_ic)*(r_ih-r_ic))
        termB = (4*(r_ic+l_v2(i))*(r_ih*r_ih - (r_ih-r_ic)*(r_ih-r_ic))) / ((r_ih*r_ih + (r_ic + l_v2(i))*(r_ic + l_v2(i)) - (r_ih - r_ic)*(r_ih - r_ic))*(r_ih*r_ih + (r_ic + l_v2(i))*(r_ic + l_v2(i)) - (r_ih - r_ic)*(r_ih - r_ic)))
        alpha_2(i) = (alpha_1*termA + omega_1*dldt_v2(i)*termB)*1e-3
        
        !open (unit=20, file="omega_2.txt")
        !open (unit=21, file="omega_3.txt")
        !open (unit=22, file="alpha_2.txt")
        !write (20,*) omega_2(i)
        !write (21,*) omega_3(i)
        !write (22,*) alpha_2(i)

460 continue
    
!    do 461 i=1, max_write_data
!        write (20,*) omega_2(i*data_step)
!        write (21,*) omega_3(i*data_step)
!        write (22,*) alpha_2(i*data_step)
!461 continue
    
end subroutine h_cylinder