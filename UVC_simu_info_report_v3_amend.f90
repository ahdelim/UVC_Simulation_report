subroutine UVC_simulation_info
    
    print *, '      ==================================================================== '
    print *, '      |                Simulation of U-Vane Compressor                   | '
    print *, '      ==================================================================== '
    print *, '      ==================================================================== '
    print *, '      |                Created by       LIM Yeu De                       | '
    print *, '      |                Created on       02-Sept-2016                     | '
    print *, '      |                Last updated on  04-FEB-2019                      | '
    print *, '      ==================================================================== '
    print *, ' '
    print *, ' --------------------------------------------------------------------------- '
    print *, ' UVC Simulation  '
    print *, ' --------------------------------------------------------------------------- '
    
    write (113,*) ' ==================================================================== '
    write (113,*) ' |                Simulation of U-Vane Compressor                   | '
    write (113,*) ' ==================================================================== '
    write (113,*) ' ==================================================================== '
    write (113,*) ' |                Created by       LIM Yeu De                       | '
    write (113,*) ' |                Created on       02-Sept-2016                     | '
    write (113,*) ' |                Last updated on  04-FEB-2019                      | '
    write (113,*) ' ==================================================================== '
    write (113,*) ' '

    endsubroutine
! (04-02-2020)  thermo_model.f90                    --- line 200~ added ',' for ease in printing
! (28-11-2019)  dynamic_m2.f90                      --- to account for dynamical radial clearance
!               journal_Bearing_2                   --- to account for dynamical radial clearance (rotor-cylinder)
!               leakage_m                           --- line 53 - added l_f_rad_ro = 2.0*pi*cl_rotor_rad_dy(i)*r_oc*1.d-3/((e*1.d-3)*sqrt(1.0 - (1.0 - cl_rotor_rad_dy(i)/(e*1.d-3))**2))
! (23-11-2019)  power_model.f90                     --- Remove the inertia terms from inst. power, line 119
! (25-06-2019)  create write file for average loss
! (22-06-2019)  leakage_model.f90                   --- Change the last section leakage to 0.2 coefficient (line 219-223)
! (20-06-2019)  Create report v3                    
! (18-06-2019)  geo_comp_volume.f90                 --- reduced suction chamber dead volume
!               mass_flow_suction_ver2.f90          --- Took out the area open
! (17-06-2019)  power_model.f90                     --- Added eccentric viscous friction (3mm gap)
!               dynamic_model.f90                   --- Added imbalance force into F_resultant
! (15-06-2019)  power_model.f90                     --- Modified T_ecc
! (14-06-2019)  power_model.f90                     --- line 97 - added indicated work to replace compression work calculated from torque.
!                                                   --- Added leakage loss
!                                                   --- Modified the eccentric frictional loss
! (14-06-2019)  leakage_model.f90                   --- line 155 - Added yanagisawa model (rotor endface leakage)
! (11-06-2019)  simulation_parameter_step_size.f90  --- reduce suction line step size
!               operational_parameter.f90           --- write out force coefficient
! (09-06-2019)  simulation_parameter_step_size.f90  --- line 45-60 change the data function
! (09-06-2019)  mass_flow_discharge_ver2.f90        --- Switch to normal mode (without port close)
! (07-06-2019)  power_model.f90                     --- Modified endface losses to include bottom endface shear stress
! (04-05-2019)  discharge_valve_deflection_ver2.f90 --- line 36,37,116,117 -- ADDED F_vis and F_ten, viscous force and surface tension, however, viscous is considered in damping ratio
!               operational_parameter.f90           --- Added sur_ten_oil and angle_oil_film for surface tension force calculation (reed valve)
!               main_dimension.f90                  --- line 110 -- Added ratio_val_port 
!               Runge_kutta.f90                     --- Line 47,118 -- Added new runge kutta equation for valve dynamic calculation
!               var_operational_parameter.f90       --- Added common/valve_para_3/sur_tension_oil, angle_oil_film, ratio_val_port
    
    
! (27-04-2019)  discharge_valve_mode_shape_ver2.f90 --- Added squared mode shape function
!                                                   --- Changed the denominator 
!                                                   --- Added condition for mode 1 and mode 2
!
! (18-04-2019)  var_power.f90                       --- Added lip seal power loss term
!               var_physical_constants.f90          --- commented out coefficient of friction terms and move into var_operational_parameter.f90
!               operational_parameter.f90           --- Added lip seal reading and write out
!               power_model.f90                     --- Lip seal power loss and torque
!               optimization_thermo.f90             --- Line 600--- added lip seal term
!
    
! (05-04-2019)  UVC_simulation_info.f90             --- Changed name to U-Vane Compressor
!               UVC_main_prototype.f90              --- Changed name to U-Vane compressor
! (28-03-2019)  discharge_valve_deflection_ver2.f90 --- Debugged for model 2
! (21-03-2019)  optimization_geometrical.f90        --- Error found in optimization parameters, creating "var_opt_parameters.f90"
!
! (12-03-2019)  discharge_valve_xxxx.f90            --- Part 3 is taken out because it is clamped by valve stop
!               discharge_valve_mode_shape_ver2.f90 --- Line 78~84 - Added rectangular 
!                                                   --- Discharge valve normal one is working fine.
!                                                   --- Constant width valve working fine.
! (07.03-2019)  optimization_main.f90           --- Line 126~134 - Changed 5F10.4 to 20F10.4
    
    
    
! (05-03-2019)  mass_flow_discharge_ver2.f90    --- Line 53 - constant "Area_port_opening"
!               mass_flow_discharge_1.f90       --- Line 26 - uncommented the "a_circumference"
!               operational_parameter.f90       --- Line 55~57 - Added leakage flow coefficient for clearances
!               heat_transfer_model.f90         --- Line 48~49  - Changed the temperature of oil sump
!               thermo_with_heat_leakage.f90    --- Line 161~164 - added condition
!               thermo_model.f90                --- Line 209~217 - commented out unneccassary error showing on screen
!               thermo_error_check.f90          --- Created new showing line
    
! (04-03-2019)  thermo_heat_leakage.f90 --- Line 164-166 - if compression chamber pressure is lower, then leakage = 0
    
! (28-02-2019)  power_model.f90     --- Line 101 - Added L_expansionloss into P_inst_total calculation 
!               --- Created version 2-2 G.A. version
    
! (24-02-2019)  optimization_main.f90           --- Added opt_idle_check to check for idle iteration (repeated iteration without any convergence found)
!                                               --- improv_check_cut_off, opt_cut_off as input to ease the process
!               optimization_geometrical.f90    --- Created subroutine 'optimization_improv_move.f90', at line 1197 of file 'optimization_geometrical.f90'
!                                               --- Created subroutine 'optimization_force_move.f90', line 1655~2108
!                                               --- arrangement bug fixed, 't_ro = r_ro - e - r_shaft' is before  'e = r_oc - r_ro'
    
! (23-02-2019)  optimization_geometrical.f90    --- fixed ori_exp(worst_index) array bug, worst_index --> worst_index(1) 
!                                               --- input/output variables (r_oc-exp1) was taken off, replace with array of variables (ori_exp1(worst_index(1))
!                                               --- Optimize set 1 geometrical checking
!                                               --- added idle checking, if idle, escape by random generated value
!               optimization_general.f90        --- Line 164~183 write parameters into file
!
!!
!
! (22-02-2019)  optimization_general.f90        --- Line 119~120 use the index to find the maximum objective function value in the array
!               optimization                    --- Adding second set of design variables for optimization, user can now select 2 sets of design variables in the begining
!               optimization_geometrical.f90    --- Line 434, factor_a & factor_b value change
!
!
! (21-02-2019)  Heat_transfer_model.f90         --- Line 930~934 adjusted the spacing, F10.4 to F15.4
!               optimization_geometrical.f90    --- Line 264  11.5 constant changed to dis_disc, r_dv_x1 = 0.5*dia_disc + 1.0 ! 
!                                               --- Other lines with the r_dv_x1, and r_vh = 0.5*l_v_ro + 1.0 changes (originally 2.0)
!                                               --- Added 
!               optimization_main.f90           --- line 18,22 - missing T_avrg_ecc fixed
!               
    
! (19-02-2019)  leakage_fanno_width.f90     --- condition of choked flow checking (line 36-37) is changed
!               leakage_fanno_width_s_clr   --- Added new subroutine for small clearance fanno width (vane endface) 
    
! (18-02-2019)  Input Main Dimensions.txt   --- Prototype dimensions are used in this version
!               working_fluid_operating_condition.f90   --- discharge temperature for air is adjusted
    
    
! (28-11-2018)  Leakage             --- assigned the average leakage mass flow calculation to a common constant first
!                                   --- make the leakage result shown in overview file
!                                   --- Updated in optimization too

! (22-11-2018)  optimization        --- Try with Prof Ooi centroid calculation method
    
! (13-11-2018)  optimization        --- Print out best and worst point, and real difference between two
!                                   --- fixed r_shaft < lower constraint bug
    
! (09-11-2018)  optimization        --- Constraints adjusted
!                                   --- Print out if constraints is violated
    
! (01-11-2018)  Exergy_main.f90     --- Added exergy transferred to fluid
!                                   --- Adjusted compressor overview print layout (power part)

! (25-10-2018)  compressor_performance.f90  --- Revised CFM calculation formula (for air compressor)
!                                           --- Total mass flow rate now subtract out average mass leakage
!               Leakage_model.f90           --- Changed error criteria to 5% (leak_conv_criterion = 0.05) 
    
! (04-10-2018)  --- Input of oil lubrication parameters changed.
!               journal_bear_hirani.f90 --- Write the analysis into overview file

! (01-10-2018)  Oil_lubrication_model version 4.f90 --- Updated.    
!                                                   --- Changed dimension of groove
    
! (30-09-2018)                      --- Modified dimensions based on SolidWorks prototype
!               oil_lubrication_model 180930 version 4.f90  --- New oil lubrication model, upper bearing groove is taken out    
    
    
! (20-09-2018)  NVC_main.f90        --- Added eccentricity and attitude angle into file writing
!               power_model.f90     --- Modified T_ecc (eccentric torque) and eccentric frictional power loss
    
! (18-09-2018)  Modified Heat Transfer model to suit new prototype model
! (11-09-2018)  Take off the changes on 180909  
!               exergy_main.f90     --- Added throttling only and mixing only equation
    
! (09-09-2018)  NVC version 2-2
!               main_dimension.f90  --- Added few parameters to address the dead volume, and the rotor vane slot gap
!               geo_comp_volume.f90 --- Added dead volume calculation method, and the rotor vane slot gap increasing volume
!               leakage_fanno.f90   --- Modified the mach_t iteration part

! (28-08-2018)  power_model.f90     --- Changed vel_avrg to r_roi*omega_1
! (24-08-2018)  exergy_main.f90 * Power_model.f90       --- Added P_bear_s into exergy analysis
!               
    
! (20-08-2018)  working_fluid_operating_condition.f90   --- Added "p_disc_air" for input air discharge pressure
!
    
! (15-08-2018)  var_compressor_evaluation.f90   --- Added mass_suct_in for volumetric efficiency (II)
!               compressor_performance.f90      --- Added volumetric efficiency (II) (discharge mass/ suction mass) 
!               thermo_with_heat_leakage.f90    --- line 212 - mass_suct_in = m_scv(no_data+1)*freq ! [kg]
    
! (28-07-2018)  exergy_main.f90 --- Coded in formula
!                               --- Coded write file 
!                               --- implement to optimization file
    
    
! (26-07-2018)  NVC Version 2-1 for exergy
!                   --- Added exergy_opt (option model ON/OFF), CPU_exergy (cpu time log)
!                   --- Modified re-expansion loss (L_avrg_reexpansion)

! (26-06-2018)  Comp_performance --- Added "CFM", evaluation for Air Compressor
    
! (22-05-2018)  Leakage model   --- Leakage is now converged in 700 seconds (Office pc)
! (21-05-2018)  Leakage model --- Bisection is now converged by using percentage instead of difference
!                             --- choked flow checking and mach_t_max
    
! (19-05-2018)  Leakage model --- fixed leakage fanno spiking problems
! (17-05-2018)  leakage model --- added bisection method as a subroutine
!                             --- Added choked flow checking
! (15-05-2018)  Refprop_ini --- Moved universal gas constant to this routine "R_universal"[J/mol.K]	! Universal Gas Constant
! (30-04-2018)  Optimization --- Created files for data writing (unit 152 - 155)
!                            --- Created routines to show the best performed point, and show in file 155
    
! (29-04-2018)  Optimization --- Created arrays for storing data, arrays are dynamically allocated (allocatable function)
!                            --- Headers before optimization starts
 
! (28-04-2018)  Optimization --- Created "optimization_performance.f90"
!                            --- Edited "optimization_indicated_work.f90"
    
! (27-04-2018)  Optimization --- Finished debugging "optimization_thermo.f90"
!                            --- geo_vanelength.f90 and gamma_1.f90 is edited to counter "NaN" occurance
!                            --- original complex can now successfully finished running without stuck at Thermo

! (24-04-2018)  Optimization --- Creating main body for optimization
!                            --- Move out journal bearing file writing
    
! (24-04-2018)  Optimization --- Created input file and output file
!                            --- Created option selection for optimization
!                            --- Optimization_main.f90
!                           --- Optimization_geometrical.f90, optimization_general.f90
!                           --- optimization subroutine series are written in one file
!                           --- var_optimization.f90

! (21-04-2018)  Setting up optimization code
! (10-04-2018)  Heat_transfer_model.f90     --- Added average oil flow velocity based on calculated oil flow rate
!                                           --- Separated bottom and lower endface oil flow
    
! (05-04-2018)  Oil_lubrication_model.f90   --- Added element pressures
!               Opening_file.f90            --- Added file unit 142
  
! (03-04-2018)  edited "Oil_lubrication_model.f90", flow path length is averaged.
    
! (02-04-2018)  Optimized "Heat_transfer_model.f90", less arrays presented to save memory
    
! (01-04-2018)  Updated L_eq_ref in leakage_model.f90
!               Added "p_reservoir" as input in Oil_lubrication_model.f90
!               Name of "Oil Lubrication Dimensions.txt" changed to "Oil lubrication parameters.txt"
    
! (31-03-2018)  Updated Heat Transfer Model from WX.
    
! (29-03-2018)  Added "open(unit=97, file="Results\\Heat Transfer Components Temp.txt")" in "Opening_file.f90"
!               Write temperatures in files in "thermo_model.f90"
    
! (28-03-2018)  Added different pitch and helix angle for eccentric in "Oil lubrication model.f90"
!       

! (27-03-2018)  Added latest "Oil_lubrication_model.f90"
!               reversed Q9b, and also added -ve sign at mat_B(7) and mat_B(8)
!               Added "open(unit=96, file="Results\\Heat Transfer Rate.txt")" in "Opening_file.f90"
!               Vane side leakage model is taken out because it is hard to model
    
! (26-03-2018)  Added "var_heat_transfer.f90" and "var_heat_transfer_gauss.f90" to keep the variables
!               Implemented Heat transfer rate in thermodynamics model.
!               Added Errors of temperature in "thermo_with_heat_leakage.f90"
!               thermal conductivity of oil is changed to 0.15 (previously was 150)
    
! (23-03-2018)  New force analysis of Vane Housing in "Dynamics Model.f90"
!               Added new dimension - Exposed clearance length of vane housing in the chamber - l_exposed_vh
!               Added "l_exposed_vh" in both "Main_dimensions.f90", "input main dimensions.txt", "var_Main_dimensions.f90"
!               Modified the definition of "F_vvh(i)"
!               Added and edited the information output when running thermodynamics model - "thermo_with_heat_leakage.f90", "thermo_no_heat_leakage.f90"
!               Added Rotor Endface Leakage Model based on CFD Simulation model (by W.C. Poh) in "leakage_model.f90"
    
! (21-03-2018)  Fixed "Oil_lubrication_model.f90" bugs, the flow rate is now working well
    
! (19-03-2018)  Added "l_dv_neck" in "main_dimension.f90" for reed valve neck length
    
! (18-03-2018)  The problem in Eq. 14 (Mat_Q_element(14,x), repeated equation possible
!               Added a matrix checking in the "Gauss_elimination.f90" subroutine
!               Modified "Opening_file.f90", input files is contained in Folder "Inputs", results in "Results", Errors in "Errors"
    
! (17-03-2018)  Running Simulation Version 1.5, to implement every model
!               Added input file "Input Oil Lubrication Dimensions.txt" for oil feeding holes parameters
!               edited "Opening_file.f90" for the input files
!               Have to debug because no value shown
    
! (16-03-2018)  Added "Oil_lubrication_model.f90"
!               Renamed "thermo_m.f90" to "thermo_model.f90"
    
! (06-03-2018)  Implemented then changes that WC has made
!               Added 'leakage_orifice.f90' in the simulation NVC 1-4
    
! (27-02-2018)  Added required oil flow calculation in journal bearing
!               Q_martin and etc (flow is at around 10cc/min)
    
! (23-02-2018)  Incomplete heat_transfer_model.f90 added in, waiting for WX to debug
    
! (01-02-2018)  Added input for Heat Transfer Model
    
! (29-01-2018)  Created "heat_transfer_model.f90", renamed simulation to version 1-3
!               
    
! (28-01-2018)  Tested existing leakage model, everything seem fine
!               Modified the decimal point to be shown in leakage_model.f90
!               Added "Var_leakage.f90"
!               Upper and Lower endface clearance are considered together

! (26-01-2018)  Data writing is added for leakage mass in "thermo_m.f90"
!               Make correction for dmdtheta1_leak at the last point no_data+1
!               Summarised all leakage mass flow into dmdtheta1_leak_s and dmdtheta1_leak_d
!               Corrected miu to be miu_disc for miu_refrigerant
!               Took off thermo model error "abs" error value
!               dmdt is now changed to dmdtheta1
    
! (24-01-2018)  vane endface leakage and radial leakage is now running smoothly
!               Added l_v_leakage to counter l_v(i) error i.e. l_v_leakage = l_v(i) + 0.0000001
!               Optimized the speed by using error_press to boost the mach_t iteration i.e. mach_t = mach_t + error*0.0001
    
! (22-01-2018)  Optimized code in leakage_model.f90
    
! (17-01-2018)  Modified code in leakage_model.f90
    
! (13-01-2018)  Added information in comment for leakage model and subroutine
    
! (23-12-2017)  Make some calculation into subroutine "leakage_lambda.f90", "leakage_channel.f90"
    
! (22-12-2017)  Implemented Bisection method to replace root-search loop for Mach_e in "leakage_model.f90"
!               mach_t changed to a smaller scale

! (21-12-2017)  Changed Floating Point setting to Precise
!               Trying out convergence of leakage model
    
! (20-12-2017)  Created "leakage_model.f90" for subroutine leakage_m
!               Added Universal Gas Constant for calculating specific gas constant
    
    
! (19-12-2017)  Added "Input model options.txt" to select which models are on
!               Output "Cv" in thermodynamic model
    
! (17-12-2017)  Try Dynamic Model with resultant forces at point R
!               T_com_C is now refers to roller FBD
    
! (07-12-2017)  Modified gamma_1 convention in v_suc equation (added "-ve" in front of gamma_1 to correct)
! (05-12-2017)  Modified "journal_bear_hirani.f90", it is now showing correct results but haven't converged yet.
!               Journal bearing iteration forced to quit. 
!               Fixed error calculation formula of journal bearing
!               Journal bearing is now working fine.

! (03-12-2017)  T_com_C now using F_cx to solve.
!               journal bearing omega_av/omega_F_resultant has problem
    
! (02-12-2017)  Solved T_com_C problem, all torques are working fine now.
!
    
! (01-12-2017)  Journal_bear_hirani.f90 is finally working fine now.
!               Edited iteration present pattern
!               Created file id = 94, "journal bearing (hirani).txt"

! (30-11-2017)  Created "journal_bear_hirani.f90" where the Hirani version Journal Bearing solving method is written there (not yet completed)
!               Added "====" line in main program

! (28-11-2017)  Added Compression efficiency (eff_comp)
!               Added COP_real, COP_w_hl 
!               Changed width of valve to "radius"
!               Added "F_resultant" in "var_dyanmic.f90"

! (27-11-2017)  Testing for correct compression power Case 1 and Case 2
!               Corrected gamma_1 convention (-ve and +ve problem)
!               Added average powers and torques to "Compressor_performance.f90", the result is shown in both txt and screen
    
! (24-11-2017)  Created "power_m.f90", power model
!               Corrected dgammadt_1 (now have smooth values)
!               Created "Powers - individual frictions.txt" and "Powers - Instantaneous.txt" (id 92 and 93)
!               Modified the file name "Forces - individual components.txt" to "Torques - individual components.txt" (id 91)
!               T_com_C, F_AB, F_v are weird, have to check
    
! (23-11-2017)  Added F_vvh in "dynamic.model.f90", the resultant pressure force acting on vane housing
!               T_roll is taken out, currently using dynamic model version 2-3 (treat F_vhoc_t as unknowns)
    
! (22-11-2017)  Added dynamic model version 2-2 (6 unknowns)
!               Modified "var_dynamic.f90" 
!               Tested version 2-0 and it works (6 unknowns with normal force acting on point O)
!               Added file id=91 --- "Forces - individual components.txt"
!               Rename "Resultant forces.txt" to "Forces - Resultant.txt"
!               Added "T_average" in compressor evaluation (average torque) in "dyanmic.model.f90"
    
! (21-11-2017)  Finished Dynamic --- resultant force model (but all zeros)
!               Deleted "var_xxxx_declare.f90" and modified "Declaring_variables.f90"
!               Edited "kine_rotor.f90", gamma_1 acceleraton is caluclated using change rate of gamma_1 velocity
!               Found bug in "Gauss_elimination.f90"
    
! (20-11-2017)  Completed the dimension variables (r_ecc, l_ecc) for easier coding purpose
!               Gauss_elimination.f90 is now with pivoting and scaling, allow for random zeros
!               added clearance at "var_clearance.f90" and "operational_parameter.f90"
!               added "var_dynamic.f90"
    
! (17-11-2017)  Completed vane housing dimensions (l_vs_ro) for dynamic model
    
! (04-11-2017)  Re-expansion model, mass flow model version 2, compression loss, AIR.PPF is now can be selected as working fluid
!               Seperate mass flow version 2 with version 1 (no heat leakage for version 1)
