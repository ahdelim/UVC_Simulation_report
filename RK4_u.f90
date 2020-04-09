subroutine RK4_un(dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, un)
    
    include "var_simulation_parameter.f90"
    integer i
    real dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, un, u
    real n
    
    un = u_cv
    du = 0
    
    do 273 i = 1,4
        
        if (i == 1) then
            u = u_cv
        elseif (i == 2 .or. i == 3) then
            u = u_cv + 0.5*du
        elseif (i == 4) then
            u = u_cv + du
        endif
    
        du = theta_step*(dqdtheta1 - (p_cv*1000)*dvdtheta1 + dedtheta1 - u*dmdtheta1)/m_cv
        
        if (i == 1 .or. i == 4) then
            n = 0.166666666666
        elseif (i == 2 .or. i == 3) then
            n = 0.333333333333
        endif
        
        un = un + n*du
273 continue
end subroutine RK4_un