subroutine open_file
    ! Input file
    open (unit = 110, file = "Inputs\\Input Main Dimensions.txt", status = 'OLD')     ! Input file for main dimensions
    open (unit = 111, file = "Inputs\\Input Step Size.txt", status = 'OLD')           ! Input file for step size
    open (unit = 112, file = "Inputs\\Input Operational Parameters.txt", status = 'OLD')        ! Input file for operational parameters
    open (unit = 114, file = "Inputs\\Input Error Criteria.txt", status = 'OLD')        ! Input parameter for convergence criteria
    open (unit = 115, file = "Inputs\\Input Model Options.txt", status = 'OLD')         ! To toggle model on/off
    open (unit = 116, file = "Inputs\\Input Oil Lubrication Parameters.txt", status = 'OLD')    ! Input dimensions for oil network feeding holes and others
    open (unit = 117, file = "Inputs\\Input Optimization parameters.txt", status = 'OLD')         ! Parameters for optimization routine
    ! Overview Data file
    open (unit = 113, file = 'Results\\Overview Performance (actual).txt')              ! File shows every information actual compressor performance
    
    ! condition
    !open (unit = 31, file = "Suction and Discharge condition.txt")
    
    ! dimension
    !open (unit = 16, file = "Main Dimensions.txt")
    
    ! parameters
    open (unit = 17, file = "Results\\Simulation Parameters Information (step size).txt")
    
    ! theta
    open (unit = 3, file = "Results\\Theta_2 vs Theta_1.txt")
    !open (unit = 4, file = "theta_3 vs theta1.txt")
    
    ! vane length
    open (unit = 12, file = "Results\\Vane Length vs Theta_1.txt")

    ! volume 1
    open (unit = 8, file = "Results\\Variation of Volume of NVC.txt")
    !open (unit = 25, file = "gamma_1 vs theta_1.txt")
    
    ! volume 2
    !open (unit = 26, file = "gamma_2 vs theta_1.txt")
    !open (unit = 5, file = "Volume_Second_Comp.txt")
    
    ! Geometric differentition
    open(unit = 61, file="Results\\Differentiated Variables.txt")
    !open(unit = 21, file="Differentiated Variables 2.txt")
    
    ! vane velocity
    open (unit = 11, file="Results\\Vane Velocity vs Theta_1.txt")
    
    ! Rotor
    !open (unit=20, file="Hollow cylinder swinging speed vs Theta_1.txt")
    open (unit=22, file="Results\\gamma_1 speed and acceleration vs Theta_1.txt")

    
    ! Ideal thermodynamic
    open (unit=40, file="Results\\Ideal_CVs_properties_1st.txt")
    !open (unit=44, file="Ideal_CVs_properties_2nd.txt")
    open (unit=43, file="Results\\Ideal_Full_cycle_properties.txt")
    open (unit=41, file="Results\\Ideal_Mass_Flow_Rate.txt")
    open (unit=45, file="Results\\Overview Ideal.txt")

    ! thermo
    !open(unit=70, file="dm-de-dv_suction_1.txt")
    open(unit=71, file="Results\\Full Cycle Thermodynamics Properties.txt")
    open(unit=72, file="Results\\PV Diagram.txt")
    open(unit=73, file="Results\\Suction CV properties.txt")
    open(unit=74, file="Results\\Compression CV properties.txt")
    open(unit=75, file="Results\\Full Cycle props w heat and leakage.txt")
    open(unit=76, file="Results\\Compression CV properties w heat and leakage.txt")
    open(unit=77, file="Results\\Suction CV properties w heat and leakage.txt")
    open(unit=78, file="Results\\PV diagram w heat and leakage.txt")
    !open(unit=79, file="dm-de-dv_discharge_1.txt")
    !open(unit=80, file="dm-de-dv_suction_2.txt")
    !open(unit=81, file="dm-de-dv_discharge_2.txt")
    open(unit=80, file="Results\\dm-de-dv.txt")
    open(unit=81, file="Results\\dm-de-dv w heat and leakage.txt")
    open(unit=82, file="Results\\Reed Valve Deflection.txt")
    open(unit=83, file="Results\\Reed Valve Deflection w heat and leakage.txt")
    !open(unit=42, file="Mass Flow Rate.txt")
    
    ! Dynamic model
    open(unit=90, file="Results\\Forces - resultant.txt")
    open(unit=91, file="Results\\Torques - individual components.txt")
    open(unit=92, file="Results\\Powers - individual frictions.txt")
    open(unit=93, file="Results\\Powers - Instantaneous.txt")
    ! Journal Bearing
    open(unit=94, file="Results\\Journal Bearing - Hirani.txt")
    open(unit=162, file="Results\\Rotor clearance.txt")
    ! Leakage Model
    open(unit=95, file="Results\\Leakage flow.txt")
    ! Heat Transfer Model
    open(unit=96, file="Results\\Heat Transfer Rate.txt")
    open(unit=97, file="Results\\Heat Transfer Components Temp.txt")
    open(unit=98, file="Results\\Heat Transfer Components Temp_2.txt")
    ! Oil lubrication Network Model
    open(unit=141, file="Results\\Oil lubrication network flow.txt")
    open(unit=142, file="Results\\Oil lubrication flow pressure.txt")
    ! Work done / Power
    open(unit=33, file="Results\\Indicated Power.txt")
    open(unit=161, file = "Results\\Exergy analysis.txt")
    !open(unit=162, file="Results\\Power - individual frictions (average).txt")
    ! Optimization parameters and results
    open(unit=151, file="Results\\Optimization parameters.txt")
    open(unit=152, file="Results\\Optimization re- obj and geometrical.txt")
    open(unit=153, file="Results\\Optimization re- power.txt")
    open(unit=154, file="Results\\Optimization re- individual losses.txt")
    open(unit=155, file="Results\\Optimization Results.txt")
    
    
    ! Testing & Error file
    open(unit=99, file="Results\\test1_99.txt")
    open(unit=100, file="Results\\test2_100.txt")
    open(unit=101, file="Results\\test3_101.txt")
    open(unit=102, file="Errors\\102_PSFLSH_error_suct_massfw.txt")
    open(unit=103, file="Errors\\103_PHFLSH_error_suct_massfw.txt")
    open(unit=104, file="Errors\\104_DEFLSH_error_suct_nextcv.txt")
    open(unit=105, file="Errors\\105_PSFLSH_d1_error_massfw.txt")
    open(unit=106, file="Errors\\106_PHFLSH_d1_error_massfw.txt")
    open(unit=107, file="Errors\\107_DEFLSH_d1_error_nextcv.txt")
endsubroutine open_file
