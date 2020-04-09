subroutine next_prop_cv(dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, v_cvn, un, mn, rhon, pn, tn, hn, sn, miun, cvn, cpn, kn, v_cv)
    
    ! Refprop
    implicit double precision (a-h,o-z)
    implicit integer (i-k,m,n)
    character(255) :: herr, hfmix
    character(3) :: hrf
    Integer, parameter :: nc = 1, ncmax=20
    dimension z(ncmax),x(ncmax),y(ncmax)
    common/fluid_info/wm
    ! Parameter
    include "var_simulation_parameter.f90"
    ! Thermo Variables
    double precision dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, v_cvn, un, mn, rhon, pn, tn, hn, sn, miun, cvn, cpn, kn
    double precision v_cv
    !call Runge_thermo_prop(dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, un)
    call Runge_thermo_prop(dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, un, v_cv, rhon)
    !call RK4_un(dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, un)
    
    ! Convert un [J/kg] to [J/mol]
    e = (un/1000.)*wm

    ! Chamber mass [kg] of the next step
    mn = m_cv + dmdtheta1*theta_step        !unit in [kg]
    !mn = rhon*v_cvn*1.e-9
    
    ! Chamber density based on next step mass [kg/m^3] 
    !rhon = mn/(v_cvn*1.e-9)
    D = rhon/wm         ![mol/L]
    call DEFLSH (D,e,z,t,p,Dl,Dv,x,y,q,h,s,cv,cp,w,ierr,herr)
    if (ierr.ne.0) write (104,*) herr
    
    
    ! Properties of the next step using "rhon" and "e" (Have to convert the unit)
    tn = t
    pn = p
    sn = s*1000./wm
    hn = h*1000./wm
    cvn = cv*1000./wm
    cpn = cp*1000./wm 
    
    ! input d
    d = D
    call TRNPRP (t,d,x,eta,tcx,ierr,herr)
    miun = eta*1.d-6
    kn = tcx
    
end subroutine next_prop_cv