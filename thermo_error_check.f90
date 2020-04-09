subroutine thermo_error_check(k, k_stop, p_scv, p_dcv, t_scv, t_dcv, p_test_suct, p_test_comp, t_test_suct, t_test_comp)
    
    integer k, k_stop
    include "var_simulation_parameter.f90"
    include "var_thermo.f90"
    include "var_thermo_error_ptr.f90"
    include "var_thermo_error_criterion.f90"
    
    
    if (k == 1) then
        p_test_suct(k) = p_scv(no_data)
        p_test_comp(k) = p_dcv(no_data)
        t_test_suct(k) = t_scv(no_data)
        t_test_comp(k) = t_dcv(no_data)
    
        !print *, '                  2nd iteration required to check convergence '
        !print *, ' ---------------------------------------------------------------------------- ' 
        !print *, ' ---------------------------------------------------------------------------- '
        !print '(2x,A,F10.2,A,F10.2,A)', 'Error in Outer Suct. Press. = ', abs(error_pos)*100.0, ' % | Disc. Pressure = ', abs(error_poc)*100.0,' %'
        !print '(2x,A,F10.2,A,F10.2,A)', 'Error in Outer Suct. Temp.  = ', abs(error_tos)*100.0, ' % | Disc. Temp.    = ', abs(error_toc)*100.0,' %'
        !print '(2x,A,F10.2,A,F10.2,A)', 'Error in Inner Suct. Press. = ', abs(error_pis)*100.0, ' % | Disc. Pressure = ', abs(error_pic)*100.0,' %'
        !print '(2x,A,F10.2,A,F10.2,A)', 'Error in Inner Suct. Temp.  = ', abs(error_tis)*100.0, ' % | Disc. Temp.    = ', abs(error_tic)*100.0,' %'
        !print *, ' ---------------------------------------------------------------------------- '
    else
        p_test_suct(k) = p_scv(no_data)
        p_test_comp(k) = p_dcv(no_data)
        t_test_suct(k) = t_scv(no_data)
        t_test_comp(k) = t_dcv(no_data)
        
        error_pos = (p_test_suct(k) - p_test_suct(k-1))/p_test_suct(k-1)
        error_poc = (p_test_comp(k) - p_test_comp(k-1))/p_test_comp(k-1)
        error_tos = (t_test_suct(k) - t_test_suct(k-1))/t_test_suct(k-1)
        error_toc = (t_test_comp(k) - t_test_comp(k-1))/t_test_comp(k-1)
        
        print *, ' ---------------------------------------------------------------------------- '
        print '(2x,A,F15.2,A,F15.2,A)', 'Last Suct. P. = ', p_test_suct(k-1), ' kPa | Current Suct. P. = ', p_test_suct(k), ' kPa'
        print '(2x,A,F15.2,A,F15.2,A)', 'Last Disc. P. = ', p_test_comp(k-1), ' kPa | Current Disc. P. = ', p_test_comp(k), ' kPa'
        print '(2x,A,F15.2,A,F15.2,A)', 'Last Suct. T. = ', t_test_suct(k-1), ' K   | Current Suct. T. = ', t_test_suct(k), ' K  '
        print '(2x,A,F15.2,A,F15.2,A)', 'Last Disc. T. = ', t_test_comp(k-1), ' K   | Current Disc. T. = ', t_test_comp(k), ' K  '
        print '(2x,A,F15.2,A,F15.2,A)', 'Error - Suct. P. = ', error_pos*100.0, ' % | Disc. P = ', error_poc*100.0,' %'
        print '(2x,A,F15.2,A,F15.2,A)', 'Error - Suct. T. = ', error_tos*100.0, ' % | Disc. T = ', error_toc*100.0,' %'
        print *, ' ---------------------------------------------------------------------------- '
        print *, ' '
    endif
        
endsubroutine thermo_error_check