subroutine kine_vane(dldt_v1, dldt_v2, dldtheta1_v1, dldtheta1_v2, dldtt_v1, dldtt_v2, theta_1)
    
    include "var_main_dimensions.f90"
    include "var_geometrical.f90"
    include "var_operational_parameter.f90"
    include "var_simulation_parameter.f90"
    include "var_kinematics.f90"
    integer i
    
    
    
    do 450 i = 1, no_data+1
        
        dldtheta1_v1(i) = (e*r_oc*sin(theta_1(i)))/sqrt(e**2 + r_oc**2 - 2*e*r_oc*cos(theta_1(i)))
        dldtheta1_v2(i) = e*sin(theta_1(i)) - ((e**2*sin(theta_1(i))*cos(theta_1(i)))/sqrt((r_ih-0.5*w_vane2)**2 - e**2*sin(theta_1(i))**2))
        
        dldt_v1(i) = omega_1*(e*r_oc*sin(theta_1(i)))/sqrt(e*e + r_oc**2 - 2*e*r_oc*cos(theta_1(i)))          ! Outer Vane sliding velocity
        dldt_v2(i) = omega_1*e*sin(theta_1(i)) * ( 1 - (e*cos(theta_1(i))/sqrt((r_ih - 0.5*w_vane2)**2 - e*e*sin(theta_1(i))**2)) )       ! Inner Vane sliding Velocity
        dldtt_v1(i) = (alpha_1*e*r_oc*sin(theta_1(i)))/sqrt(e**2 + r_oc**2 - 2*e*r_oc*cos(theta_1(i))) + omega_1**2*e*r_oc*( cos(theta_1(i))/sqrt(e**2 + r_oc**2 - 2*e*r_oc*cos(theta_1(i))) + e**2 *r_oc**2 *sin(theta_1(i))**2 / ((e**2 + r_oc**2 - 2*e*r_oc*cos(theta_1(i)))*sqrt(e**2 + r_oc**2 - 2*e*r_oc*cos(theta_1(i)))) )
        dldtt_v2(i) = alpha_1*e*sin(theta_1(i))*(1 - e*cos(theta_1(i))/sqrt((r_ih - 0.5*w_vane2)**2 - e**2*sin(theta_1(i))**2)) + omega_1**2*e**2*(cos(theta_1(i))/e - cos(theta_1(i))**2/sqrt((r_ih - 0.5*w_vane2)**2 - e*e*sin(theta_1(i))**2) - (e*e - (r_ih - 0.5*w_vane2)**2)*sin(theta_1(i))**2/((sqrt((r_ih - 0.5*w_vane2)**2 - e*e*sin(theta_1(i))**2))*((r_ih - 0.5*w_vane2)**2 - e*e*sin(theta_1(i))**2))
        
        termD = 1- (((r_oc**2 + (r_oh+l_v1(i))**2 - e**2)/(2*r_oc*(r_oh+l_v1(i))))*((r_oc**2 + (r_oh+l_v1(i))**2 - e**2)/(2*r_oc*(r_oh+l_v1(i)))))
        
        if (termD == 0) then
            termD = 0.00001
        endif
        
        dgammadtheta1_1(i) = dldtheta1_v1(i)*(((r_oc**2 + (r_oh+l_v1(i))**2 - e**2)/(2*r_oc*(r_oh+l_v1(i))**2)) - 1./r_oc)/sqrt(termD)
        
        termA = 1 - ((e**2 + (r_ih-0.5*w_vane2)**2 - (r_ih+l_v2(i)-0.5*w_vane2)**2)/(2*e*(r_ih-0.5*w_vane2)))*((e*e + (r_ih-0.5*w_vane2)**2 - (r_ih+l_v2(i)-0.5*w_vane2)**2)/(2*e*(r_ih-0.5*w_vane2)))
        termB = 1 - (((r_ih-0.5*w_vane2)**2 + (r_ih+l_v2(i)-0.5*w_vane2)**2 - e**2)/(2*(r_ih-0.5*w_vane2)*(r_ih+l_v2(i)-0.5*w_vane2)))*(((r_ih-0.5*w_vane2)**2 + (r_ih+l_v2(i)-0.5*w_vane2)**2 - e*e)/(2*(r_ih-0.5*w_vane2)*(r_ih+l_v2(i)-0.5*w_vane2)))
        termC = ((r_ih-0.5*w_vane2)**2 + (r_ih+l_v2(i)-0.5*w_vane2)**2 - e*e)/(2*(r_ih-0.5*w_vane2)*(r_ih+l_v2(i)-0.5*w_vane2)**2) - 1/(r_ih - 0.5*w_vane2)
        
        dgammadtheta1_2(i) = dldtheta1_v2(i)*(termC)/sqrt(termB)
        dthedtheta1_3(i) = dldtheta1_v1(i)*(1/e + (r_oc**2 - (r_ih + t_h + l_v1(i))**2 - e**2)/(2*e*(r_ih + t_h + l_v1(i))**2))/sqrt( 1-((r_oc**2 - (r_ih+t_h+l_v1(i))**2 - e*e)/(2*e*(r_ih + t_h + l_v1(i)))) )
        
        dthedtheta1_2(i) = dldtheta1_v2(i)*(r_ih+l_v2(i)-0.5*w_vane2)/(e*(r_ih-0.5*w_vane2)*sqrt(termA))
        
        dvdtheta1_suc1(i) = 0.5*(r_oc**2 - r_oh**2*dthedtheta1_3(i) - e*(l_v1(i)+r_oh)*cos(theta_3(i))*dthedtheta1_3(i) - e*sin(theta_3(i))*dldtheta1_v1(i) - w_vane1*dldtheta1_v1(i) + 0.25*w_vane1**2 *(1/cos(gamma_1(i)))**2*dgammadtheta1_1(i) )*l_com1
        dvdtheta1_com1(i) = 0.5*(-r_oc**2 + r_oh**2*dthedtheta1_3(i) + e*(l_v1(i)+r_oh)*cos(theta_3(i))*dthedtheta1_3(i) + e*sin(theta_3(i))*dldtheta1_v1(i) - w_vane1*dldtheta1_v1(i) + 0.25*w_vane1**2 *(1/cos(gamma_1(i)))**2*dgammadtheta1_1(i) )*l_com1
        
        dvdtheta1_suc2(i) = 0.5*( r_ih**2*dthedtheta1_2(i) - r_ic**2 - e*(l_v2(i) + r_ic - 0.5*w_vane2)*cos(theta_1(i)) - e*sin(theta_1(i))*dldtheta1_v2(i) - w_vane2*dldtheta1_v2(i) + 0.25*w_vane2**2 *dgammadtheta1_2(i) )*l_com2
        dvdtheta1_com2(i) = 0.5*( -r_ih**2*dthedtheta1_2(i) + r_ic**2 + e*(l_v2(i) + r_ic - 0.5*w_vane2)*cos(theta_1(i)) + e*sin(theta_1(i))*dldtheta1_v2(i) - w_vane2*dldtheta1_v2(i) - 0.25*w_vane2**2 *dgammadtheta1_2(i) )*l_com2
450 continue
    
       
    
2985 format (11A15)
2986 format (F15.4, 10ES15.6) 
end subroutine kine_vane