      subroutine Runge_valve(g_r, h_r, f_mid, integral_f, p_cv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, g_grav, omega_response_dv, gn, hn)
!     implicit real*8 (A-H,O-Z)
      implicit none
      integer, parameter :: N=1
      integer I
      double precision, DIMENSION(1:2) :: YOUT, GRAD
      !DIMENSION :: YOUT(2)
      !DIMENSION :: GRAD(2)
      double precision g, h, dgdt, dhdt, gn, hn
      double precision g_r, h_r, time_step, f_mid, integral_f, p_cv, p_disc, area_port, rho_dv, A_cross_dv, dpratio_dv, g_grav, omega_response_dv
    
      External Derivs_valve
      
!!     -------------
!!     Example: y(t)=4*t*t*t + 2*t + 4
!c              
!c     Differential eqn:    dydt=12*t*t+2
!c     initial condition: t=0, y(t=0)=4.0, dydt(t=0)=2.0
!c     using RK4 solve the above to obtain results for y(t) at any t, and
!c     compare with the exact solution of y(t)=t*t*t+3*t+2
!c     ----------------
!      dg_r/dt = h_r
!      dh_r/dt = f_mid*(p_cv - p_disc)*1000*area_port/(rho_dv*A_cross_dv) - 2*dpratio_dv*omega_response_dv*h_r - (omega_response_dv)*(omega_response_dv)*g_r
!c     ---------
!c     Set initial condition
!c     ---------
      !t=0.0
      !y(1)=4.0
      !dydt(1)=2.0

      g=g_r             ! phi2
      h=h_r
	  dgdt = h_r        ! phi1
      !dhdt = f_mid*(p_cv - p_disc)*1000*area_port/(rho_dv*A_cross_dv) - 2*dpratio_dv*omega_response_dv*h - (omega_response_dv)*(omega_response_dv)*g
      dhdt = f_mid*(p_cv - p_disc)*1000.0*area_port/(rho_dv*A_cross_dv) - integral_f*g_grav - 2.0*dpratio_dv*omega_response_dv*h - ((omega_response_dv)**2)*g
!c     --------
!c     H = Time step
!c     --------
      !H_t = time_step
      
       
!c     ----------
!c     Call to initialise the coefficients in the numerical scheme
!c     ----------	
      !call RK4COEF_1(H_t)
	!Icount=0
!     100   continue	
    
!c     ---------
!c     Call to perform the numerical integration
!c     Y= the initial value (on return from call, it also carries the solution, same sa Yout)
!c     Yout = the solution
!c     N= number of differential equations
!c     t=time in second
!c     H=time step in second
!c     Derivs= subroutine consists of the derivatives of the Y (the differential equations) 
!c     ---------
      ! call RK4(Y,dydt,N,t,H,YOUT,DERIVS)
      
      call RK4_valve(g,h,g_r,h_r,dgdt,N,dhdt,f_mid, integral_f, p_cv, p_disc,area_port, rho_dv, A_cross_dv, dpratio_dv, g_grav, omega_response_dv, YOUT, gn, hn)
      
!c     --------
!c     increase to the next time
!c     --------
	  !t=t+H
      !theta1 = theta1 + H
!c     --------
!c     this is the excaxt solution that we will not have in real life problems.
!c     --------
	!yy=4*t*t*t+2.0*t+4. 
	!err=(yy-y(1))/yy*100  ! this is the error in percentage
     

!200   format(1x,4(F15.6,1x))     ! write using this write format
      
      !Icount=Icount+1
      !if(Icount*H.GE.3) then
      !pause
      !Icount=0
      !endif
      
	!goto 100
      endsubroutine

!C(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)*)(*)(*)(*)(*)(*)
!c     ------------
!c     these are the differential equations
!c     ------------	 
	 !Subroutine Derivs(t,YT,DYT,N)
     
     !Subroutine Derivs_valve(dt,g,h,f_mid, p_cv,p_disc,area_port,rho_dv,A_cross_dv,dpratio_dv,omega_response_dv, dgt, dht)
     Subroutine Derivs_valve(dt,g,h,f_mid, integral_f, p_cv,p_disc,area_port,rho_dv,A_cross_dv,dpratio_dv,g_grav,omega_response_dv, dgt, dht)
       implicit none
!c     implicit real*8 (A-H,O-Z)
       double precision dt,g,h,f_mid,integral_f,p_cv,p_disc,area_port,rho_dv,A_cross_dv,dpratio_dv,g_grav,omega_response_dv
       double precision dgt, dht
       !dyt(1)=12.0*t*t+2.0
       
       dgt = dt*h
       !dht = dt*(f_mid*(p_cv - p_disc)*1000*area_port/(rho_dv*A_cross_dv) - 2*dpratio_dv*omega_response_dv*h - (omega_response_dv)*(omega_response_dv)*g)
       dht = dt*(f_mid*(p_cv - p_disc)*1000.0*area_port/(rho_dv*A_cross_dv) - integral_f*g_grav - 2.0*dpratio_dv*omega_response_dv*h - ((omega_response_dv)**2)*g)
    
	 endsubroutine Derivs_valve

! C(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)*)(*)(*)(*)(*)(*)
      SUBROUTINE RK4_valve(g,h,g_r,h_r,dgdt,N,dhdt,f_mid, integral_f, p_cv, p_disc,area_port, rho_dv, A_cross_dv, dpratio_dv, g_grav, omega_response_dv, YOUT, gn, hn)
!c       implicit real*8 (A-H,O-Z)
      implicit none
      integer N
      integer I
      double precision, DIMENSION(1:2) :: YOUT, GRAD
      !DIMENSION :: GRAD(2)
      double precision t, dgtM, dhtM, dgtN, dhtN, dgt, dht
      double precision g,h,g_r,h_r,dgdt,dhdt,f_mid, integral_f, p_cv, p_disc,area_port, rho_dv, A_cross_dv, dpratio_dv, g_grav, omega_response_dv
      double precision gn, hn
      include "var_runge_steps_coefficients.f90"
      double precision a1,a2,a3,b11,b21,b22,b31,b32,b33,w1,w2,w3,w4,x1,x2,x3
      common/rk4coef1/a1,a2,a3,b11,b21,b22,b31,b32,b33,w1,w2,w3,w4,x1,x2,x3
      
       DO 11 I=1,N
        !YT(I)=Y(I)+b11*H*dydt(I)
        g = g_r + b11*H_time*dgdt
        h = h_r + b11*H_time*dhdt
        
11     CONTINUE
       !CALL DERIVS(t+X1,YT,DYT,N)
       
       call Derivs_valve(t+x1,g,h,f_mid, integral_f, p_cv,p_disc,area_port,rho_dv,A_cross_dv,dpratio_dv,g_grav,omega_response_dv,dgt,dht)
       DO 12 I=1,N
        
        !YT(I)=Y(I)+H*(b21*dydt(I)+ b22*DYT(I))
        g = g_r + H_time*(b21*dgdt + b22*dgt)
        h = h_r + H_time*(b21*dhdt + b22*dht)
        
12     CONTINUE
       
       !CALL DERIVS(t+X2,YT,DYM,N)
       
       call Derivs_valve(t+x2,g,h,f_mid, integral_f, p_cv,p_disc,area_port,rho_dv,A_cross_dv,dpratio_dv,g_grav,omega_response_dv, dgtM, dhtM)
       DO 13 I=1,N
        g = g_r + H_time*(b31*dgdt + b32*dgt + b33*dgtM)
        h = h_r + H_time*(b31*dhdt + b32*dht + b33*dhtM)
        
        !YT(I)=Y(I)+H*(b31*dydt(I)+ b32*DYT(I)+ b33*DYM(I) )
13     CONTINUE
       !CALL DERIVS(t+X3,YT,DYN,N)
              
       call Derivs_valve(t+x3,g,h,f_mid, integral_f, p_cv,p_disc,area_port,rho_dv,A_cross_dv,dpratio_dv,g_grav,omega_response_dv, dgtN, dhtN)
       DO 14 I=1,N
           
!C      ------------------------------------
!C      GRAD=1/6 *(dydt(I)+DYT(I)+2.*DYM(I))
!C      ------------------------------------
        !GRAD(I)=W1*dydt(I) + W2*DYT(I) + W3*DYM(I) + W4*DYN(I)
        GRAD(1)=W1*dgdt + W2*dgt + W3*dgtM + W4*dgtN
        GRAD(2)=W1*dhdt + W2*dht + W3*dhtM + W4*dhtN
        YOUT(1)=g_r+GRAD(1)*H_time/6.0
        YOUT(2)=h_r+GRAD(2)*H_time/6.0
        gn = g_r+GRAD(1)*H_time/6.0
        hn = h_r+GRAD(2)*H_time/6.0
        
14    CONTINUE
              
!C     --------------------------------------
!C     ADD IN BY KT OOI
!C     SET POINT FOR THE NEXT STEP
!C     --------------------------------------
      !DO 2000 I=1,N
      !Y(I)=YOUT(I)
      !u(I)=YOUT(I)          
!2000  CONTINUE
      
      !CALL DERIVS(dtheta1+H,u,dydt,N)
      RETURN
      END
         
!C(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)*)(*)(*)(*)(*)(*)
!      SUBROUTINE RK4COEF_1(H)
!!c      implicit real*8 (A-H,O-Z)
!      double precision A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3 
!      COMMON/RK4COEF1/A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3
!      CHARACTER*12 FILE1
!      double precision H              
!      CLOSE(1)
!      
!      FILE1='RUNGE.RK4'
!
!!C     -----------------
!!C     SET IOPT=-2 AS A DEFAULT FOR THE SECOND CALL TO CHANGE THE
!!C     COEFF. A1,A2,A3
!!C     -----------------
!                       
!      ! PRINT*,' READING NUMERICAL COEFF..'
!      OPEN(1,FILE=FILE1,STATUS='OLD')
!                                     
!      READ(1,*)A1
!      READ(1,*)A2
!      READ(1,*)A3
!      READ(1,*)B11
!      READ(1,*)B21
!      READ(1,*)B22
!      READ(1,*)B31
!      READ(1,*)B32
!      READ(1,*)B33
!      READ(1,*)W1
!      READ(1,*)W2
!      READ(1,*)W3
!      READ(1,*)W4
!      CLOSE(1)
!      X1=H*A1
!      X2=H*A2
!      X3=H*A3
!      
!!c      IF(IOPT.GT.0)PRINT*,'13 COEFF READ FROM FILE:',FILE1
!      RETURN
!      END
