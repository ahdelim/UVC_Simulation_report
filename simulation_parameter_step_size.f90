subroutine simulation_parameter
    
    include "var_physical_constants.f90"
    include "var_simulation_parameter.f90"

    !theta_start = 0
    !theta_end = 360
    !theta_step_degree = (theta_end - theta_start)/no_data
    !theta_step = theta_step_degree*(pi/180)
    
    read (111,*) max_write_data             ! = 10000     ! Maximum data to write into file
    
    ! ------ Parameter definition ----- !
    read (111,*) theta_step_degree_1        ! normal step size
    read (111,*) theta_step_degree_low      ! large step size
    read (111,*) theta_step_degree_high     ! small step size (more detailed)
    read (111,*) theta_start                ! start point
    read (111,*) theta_end                  ! end point
    
    !theta_step_degree_1 = 0.2
    !theta_step_degree_low = 0.5
    !theta_step_degree_high = 0.02
    !theta_start = 0.0
    !theta_end = 360.0
    
    ! ------ different number of data ------ !
    no_data = (theta_end - theta_start)/theta_step_degree_1     ! number of data point (normal)
    no_data_low = (theta_end - theta_start)/theta_step_degree_low   ! large step size (less data points)
    no_data_high = (theta_end - theta_start)/theta_step_degree_high     ! small step size (more data points)
    
    !print*, ' ------------------------------------------- '
    !print*, ' Total data points     = ', no_data
    !print*, ' ------------------------------------------- '
    
    ! ------ low to high level step size ------- !
    theta_step = theta_step_degree_1*(pi/180.0)         ! [rad]
    theta_step_low = theta_step_degree_low*(pi/180.0)   ! [rad]
    theta_step_high = theta_step_degree_high*(pi/180.0) ! [rad]
    
    data_step = no_data/max_write_data      ! step size, result in writting number of data points defined in max_write_data
    data_step_suct = (no_data/max_write_data)*1     ! write data for suction (large step), result in only 100 data points write
    data_step_high = no_data_high/max_write_data 
    !data_step = floor(data_step)
    !data_step_suct = floor(data_step_suct)
    !data_step_high = floor(data_step_high)
    
    
    write (17, 2989), 'Theta Step = ', theta_step, " rad"
    write (17, 2989), 'Theta Step = ', theta_step_degree_1, " degree"
    write (17, 2989), 'Theta Step Low = ', theta_step_low, " rad"
    write (17, 2989), 'Theta Step Low = ', theta_step_degree_low, " degree"
    write (17, 2989), 'Theta Step High = ', theta_step_high, " rad"
    write (17, 2989), 'Theta Step High = ', theta_step_degree_high, " degree"
    write (17, 2989), 'Start Angle = ', theta_start, " degree"
    write (17, 2989), 'End Angle   = ', theta_end, " degree"
    write (17, 2990), 'Number of Data Points = ', no_data
    write (17, 2990), 'Number of Written Points = ', max_write_data
    write (17, 2989), 'Number of Written Points Step = ', data_step
    write (17, 2989), 'Number of Written Points Step Low = ', data_step_suct
    write (17, 2989), 'Number of Written Points Step Low = ', data_step_high
    
    write (113,*) " -------------------------------------------------------------------- "
    write (113,*) " Simulation Parameters Settings "
    write (113,*) " -------------------------------------------------------------------- "
    write (113,2989) " Step size                theta_step_degree_1       = ", theta_step_degree_1, " Degree"
    write (113,2990), ' Number of Data Points                              = ', no_data
    write (113,*) ' '
    
2989 format (1x,A,F8.4,A)
2990 format (1x,A,I8)    
end subroutine simulation_parameter