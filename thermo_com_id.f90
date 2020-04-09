subroutine thermo_com_id(theta_1, v_suc, v_com, p_cscv_id, p_cdcv_id)
    
    ! REFPROP variables
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: fluid, herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1, ncmax = 20
    dimension z(ncmax),x(ncmax),y(ncmax)
    common/fluid_info/wm
    
    ! Main program variables
    include "var_operational_parameter.f90"
    include "var_operating_fluid_condition.f90"
    include "var_simulation_parameter.f90"
    include "var_physical_constants.f90"
    include "var_compressor_evaluation.f90"
    
    double precision, dimension (1:no_data+1) :: v_suc, v_com, theta_1
    double precision, dimension (1:no_data+1) :: p_cscv_id, p_cdcv_id, t_cscv_id, t_cdcv_id, m_cscv_id, m_cdcv_id, rho_cdcv_id

    ! Suction process (Pressure)
    write(40,2981) "Degree", "Suc.Volume[m3]", "Comp.Volume[m3]","I.Suct.Press._1[kPa]", "I.Suct.Temp.[K]", "I.Disc.Press.[kPa]", "I.Disc.Temp.[K]"
    write(43,2981) "Degree", "Volume[m3]","I.Pressure[kPa]", "I.Temperature[K]", "I.Mass[kg]"
    
    ! --------------- Process ------------ !
    do 261 i = 1, no_data+1
        t_cscv_id(i) = t_suc
        p_cscv_id(i) = p_suc       ! suction Pressure
        m_cscv_id(i) = rho_suc * v_suc(i)*1.d-9     ! ideal suction mass [kg]
        
261  continue
    
    !m_cdcv_id(1) = rho_suc * vol_total * 1.d-9       ! total mass [kg]
    m_cdcv_id(1) = m_cscv_id(no_data+1)       ! total mass [kg]
    
    ! Compression process of 1st compressor (Pressure and Temperature changing)
    do 262 i = 1, no_data+1
        rho_cdcv_id(i) = m_cdcv_id(1)/(v_com(i)*1.d-9)    ! Density change
        m_cdcv_id(i) = m_cdcv_id(1)                         ! Mass remain, no mass discharge before reaching disc. pressure
        D = rho_cdcv_id(i)/wm                               ! Assign density (mol/L)
        s = s_suc/1000 * wm                                ! Isentropy process (so entropy is the same after compressed) (J/(mol.K))
        call DSFLSH (D,s,z,t,p,Dl,Dv,x,y,q,e,h,cv,cp,w,ierr,herr)    ! calculate temperature and pressure 
        
        p_cdcv_id(i) = p
        t_cdcv_id(i) = t
        
        if (p_cdcv_id(i) > p_disc)  then
            p_cdcv_id(i) = p_disc
            t_cdcv_id(i) = t_disc
            m_cdcv_id(i) = m_cdcv_id(i)
            rho_cdcv_id(i) = rho_cdcv_id(i)
            exit
        endif
        
262 continue
        
        if (p_cdcv_id(i) == p_disc) then
            do 263 i = i, no_data+1
                rho_cdcv_id(i) = rho_cdcv_id(i-1)
                p_cdcv_id(i) = p_disc
                t_cdcv_id(i) = t_disc
                m_cdcv_id(i) = m_cdcv_id(i-1) - rho_cdcv_id(i-1)*(v_com(i-1)-v_com(i))*1.d-9
263         continue
        endif
        
! ------------------ Before Evaporator ------------------- !
    j = 1
    call TPRHO (t_high,p_disc,x,j,0,d,ierr,herr)
    call THERM (t_high,d,x,p,e,h,s,cv,cp,w,hjt)
    !call TPFLSH (t,p,z,D,Dl,Dv,x,y,q,e,h,s,cv,cp,w,ierr,herr)
    !print*,d
    h_bef_evap = h/wm*1000              ! [J/kg] throttle valve suction H = discharge H (before evaporator)
    !print*, h_bef_evap
    !pause
    ! -------------- write file --------------
        !do 267 i = 1, max_write_data, data_step_suct
        do 267 i = 1, no_data, data_step_suct
            write (43,2982) theta_1(i)*180.0/pi, v_suc(i)*1.d-9, p_cscv_id(i), t_cscv_id(i), m_cscv_id(i)
            write (40,2982) theta_1(i)*180.0/pi, v_suc(i)*1.d-9, v_com(i)*1.d-9, p_cscv_id(i), t_cscv_id(i), p_cdcv_id(i), t_cdcv_id(i)
            
267     continue
        
        !do 268 i = 1, max_write_data, data_step
        do 268 i = 1, no_data, data_step
            write (43,2982) 360.0+theta_1(i)*180.0/pi, v_com(i)*1.d-9, p_cdcv_id(i), t_cdcv_id(i), m_cdcv_id(i)
268     continue
        
        
        m_flow_total_id = m_cdcv_id(1)*freq
        q_capacity_id = m_flow_total_id * (h_suc - h_bef_evap)
        write(41,2981) 'Mass_flow_rate_total_id[kg]'
        write(41,2983) m_flow_total_id
        write (45,*) " -------------------------------------------------------------------- "
        write(45,*)  ' Ideal Overview  '
        write (45,*) " -------------------------------------------------------------------- "
        write(45,2990) 'Total Ideal Mass Flow Rate                m_flow_total_id        = ', m_flow_total_id, ' kg/s'
        write(45,2990) 'Ideal Cooling Capacity                    q_capacity_id          = ', q_capacity_id, ' W'
        
        
2981 format (16A35)
2982 format (F35.4, 14ES35.6) 
2983 format (14ES35.6)  
2989 format (2x,A,F8.4,A)
2990 format (2x,A,f15.4,A)
end subroutine thermo_com_id