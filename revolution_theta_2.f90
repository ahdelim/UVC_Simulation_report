subroutine revolution_theta_2(theta_2, theta_1, r_ih, e, r_ic, l_v2, no_data)
    
    real, parameter :: pi = 3.14159265359
    real, dimension (1:no_data+1) :: theta_2, theta_1, l_v2
    integer i
    
    do 250 i = 1, no_data+1
        !    theta_2(i) = acos((e*e + r_ih*r_ih - (r_ic + l_v2(i))*(r_ic + l_v2(i)))/(2*e*r_ih))
        if (i < (no_data/2 + 1))  then
            theta_2(i) = acos((e*e + r_ih*r_ih - (r_ic + l_v2(i))*(r_ic + l_v2(i)))/(2*e*r_ih))
        else if (i == (no_data / 2 + 1)) then
            theta_2(i) = theta_1(i)
        else
            theta_2(i) = 2*pi - acos((e*e + r_ih*r_ih - (r_ic + l_v2(i))*(r_ic + l_v2(i)))/(2*e*r_ih))
        endif
        open (unit=3, file="theta_2.txt")
        write (3,*) theta_2(i)
250  continue
        write (3,*) theta_2(i)
end subroutine revolution_theta_2