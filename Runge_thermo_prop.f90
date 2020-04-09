!================================================================================================================================
! Runge-Kutta Iteration
! Remarks:
! y(i+1) = y(i) + h*(k1 + 2k2 + 2k3 + k4)/6 = y(i) + h*GRAD
! k1 = f(t,y(i))
! k2 = f(t+0.5*h , y(i) + 0.5*k1*h)
! k3 = f(t+0.5*h , y(i) + 0.5*k2*h)
! k4 = f(t+h     , y(i) + k3*h)   
!===============================================================================================================================
    
    subroutine Runge_thermo_prop(dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, un, v_cv, rhon)
!     implicit real*8 (A-H,O-Z)
      implicit none
      integer, parameter :: N=1, Nmax=2
      double precision, DIMENSION(1:N) :: dudtheta1,u
      double precision, DIMENSION(1:Nmax) :: dudtheta1M, dudtheta1N, YOUT, GRAD
      double precision dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, un, theta1
      double precision drhodtheta1, rho, v_cv, rhon
      include "var_runge_steps_coefficients.f90"
      include "var_simulation_parameter.f90"
      External Derivss

!!     -------------
!!     Example: y(t)=4*t*t*t + 2*t + 4
!c              
!c     Differential eqn:    dydt=12*t*t+2
!c     initial condition: t=0, y(t=0)=4.0, dydt(t=0)=2.0
!c     using RK4 solve the above to obtain results for y(t) at any t, and
!c     compare with the exact solution of y(t)=t*t*t+3*t+2
!c     ----------------
!      
!c     ---------
!c     Set initial condition
!c     ---------
      !t=0.0
      !y(1)=4.0
	!dydt(1)=2.0

	  theta1=0.0
      u(1)=u_cv       ! [J/kg]
      rho = m_cv/v_cv*1.d9   ! [kg/m3]
	  dudtheta1(1)= H_theta*(dqdtheta1-(p_cv*1000.0)*dvdtheta1+dedtheta1-u_cv*dmdtheta1)/m_cv       ! Calculating du
      drhodtheta1 = H_theta*(dmdtheta1 - rho*dvdtheta1)/v_cv*1.d9
      
      
!c     --------
!c     H = Time step
!c     --------
        
      !H = theta_step
    
!c     ----------
!c     Call to initialise the coefficients in the numerical scheme
!c     ----------	
      !call RK4COEF(H)
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
      call RK4(u,dudtheta1,N,theta1,un, dqdtheta1, dmdtheta1, dedtheta1,p_cv, dvdtheta1, m_cv, u_cv, rho, v_cv, drhodtheta1, rhon)

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
     ! Write(99,200) un ! write them on screen to see

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
     
     Subroutine Derivss(dtheta1, dudthe, N, p_cv, dvdtheta1, dedtheta1, u_cv,dqdtheta1, dmdtheta1, m_cv, drho, rho, v_cv)
     implicit none
!c     implicit real*8 (A-H,O-Z)
       integer N
       double precision p_cv, dvdtheta1, dedtheta1, u_cv, dmdtheta1, dqdtheta1, dtheta1, m_cv
       double precision, dimension (1:N) :: dudthe
       double precision drho, rho, v_cv
       !dyt(1)=12.0*t*t+2.0
       
       dudthe(1) = dtheta1*(dqdtheta1 - (p_cv*1000.)*dvdtheta1 + dedtheta1 - u_cv*dmdtheta1)/m_cv       ! Find du
       drho = dtheta1*(dmdtheta1 - rho*dvdtheta1)/v_cv*1.d9                                             ! Find drho
       
	 endsubroutine Derivss

! C(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)*)(*)(*)(*)(*)(*)
      SUBROUTINE RK4(u,dudtheta1,N,dtheta1,un, dqdtheta1, dmdtheta1,dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, rho, v_cv, drhodtheta1, rhon)
!c       implicit real*8 (A-H,O-Z)
      implicit none
      integer N, I
      integer, PARAMETER :: Nmax=2
      double precision, DIMENSION(1:N) :: dudtheta1, dudthe, u
      double precision, DIMENSION(1:Nmax) :: dudtheta1M, dudtheta1N, YOUT, GRAD
      double precision dqdtheta1, dmdtheta1, dedtheta1, p_cv, dvdtheta1, m_cv, u_cv, dtheta1, un
      double precision rho, v_cv, drhodtheta1, rhon, drhodthe, drhodtheta1M, drhodtheta1N
      include "var_runge_steps_coefficients.f90"
      double precision a1,a2,a3,b11,b21,b22,b31,b32,b33,w1,w2,w3,w4,x1,x2,x3
      common/rk4coef2/a1,a2,a3,b11,b21,b22,b31,b32,b33,w1,w2,w3,w4,x1,x2,x3
        
       DO 11 I=1,N
        !YT(I)=Y(I)+b11*H*dydt(I)
11     CONTINUE
        !u_cv = u(1) + b11*H_theta*dudtheta1(1)          ! update u_cv
        u_cv = u(1) + b11*dudtheta1(1)          ! update u_cv
        !rho = m_cv/v_cv*1.d9 + b11*H_theta*drhodtheta1       ! update rho
        rho = m_cv/v_cv*1.d9 + b11*drhodtheta1       ! update rho
       !CALL DERIVS(t+X1,YT,DYT,N)
       call Derivss(dtheta1+x1,dudthe,N, p_cv, dvdtheta1, dedtheta1, u_cv,dqdtheta1, dmdtheta1, m_cv, drhodthe, rho, v_cv)
       DO 12 I=1,N
        !u_cv = u(1) + H_theta*(b21*dudtheta1(1) + b22*dudthe(1))        ! update u_cv
        u_cv = u(1) + (b21*dudtheta1(1) + b22*dudthe(1))        ! update u_cv
        !rho = m_cv/v_cv*1.d9 + H_theta*(b21*drhodtheta1 + b22*drhodthe)      ! update rho
        rho = m_cv/v_cv*1.d9 + (b21*drhodtheta1 + b22*drhodthe)      ! update rho
        !YT(I)=Y(I)+H*(b21*dydt(I)+ b22*DYT(I))
12     CONTINUE
       !CALL DERIVS(t+X2,YT,DYM,N)
       
       call Derivss(dtheta1+x2,dudtheta1M,N, p_cv, dvdtheta1,dedtheta1 ,u_cv,dqdtheta1, dmdtheta1, m_cv, drhodtheta1M, rho, v_cv)
       DO 13 I=1,N
        !u_cv = u(1) + H_theta*(b31*dudtheta1(1) + b32*dudthe(1) + b33*dudtheta1M(1))            ! update u_cv
        u_cv = u(1) + (b31*dudtheta1(1) + b32*dudthe(1) + b33*dudtheta1M(1))            ! update u_cv
        !rho = m_cv/v_cv*1.d9 + H_theta*(b31*drhodtheta1 + b32*drhodthe + b33*drhodtheta1M)           ! update rho
        rho = m_cv/v_cv*1.d9 + (b31*drhodtheta1 + b32*drhodthe + b33*drhodtheta1M)           ! update rho
        !YT(I)=Y(I)+H*(b31*dydt(I)+ b32*DYT(I)+ b33*DYM(I) )
13     CONTINUE
       !CALL DERIVS(t+X3,YT,DYN,N)
       
       call Derivss(dtheta1+x3,dudtheta1N,N,p_cv, dvdtheta1, dedtheta1,u_cv,dqdtheta1, dmdtheta1, m_cv, drhodtheta1N, rho, v_cv)
       DO 14 I=1,N
    
!C      ------------------------------------
!C      GRAD=1/6 *(dydt(I)+DYT(I)+2.*DYM(I))
!C      ------------------------------------
        !GRAD(I)=W1*dydt(I) + W2*DYT(I) + W3*DYM(I) + W4*DYN(I)
        GRAD(1)= W1*dudtheta1(I) + W2*dudthe(I) + W3*dudtheta1M(I) + W4*dudtheta1N(I)       ! calculate gradient
        GRAD(2)= W1*drhodtheta1 + W2*drhodthe + W3*drhodtheta1M + W4*drhodtheta1N           ! calculate gradient
        !YOUT(1)= u(1)+GRAD(1)*H_theta/6.
        !YOUT(2)= rho+GRAD(2)*H_theta/6.
        !un = u(I)+GRAD(I)*H_theta/6.        ! [J/kg]
        !rhon = rho+GRAD(2)*H_theta/6.       ! [kg/m3]
        YOUT(1)= u(1)+GRAD(1)/6.
        YOUT(2)= rho+GRAD(2)/6.
        un = u(I)+GRAD(I)/6.        ! [J/kg]
        rhon = rho+GRAD(2)/6.       ! [kg/m3]
        
14    CONTINUE
              
!C     --------------------------------------
!C     ADD IN BY KT OOI
!C     SET POINT FOR THE NEXT STEP
!C     --------------------------------------
      DO 2000 I=1,N
      !Y(I)=YOUT(I)
      !u(I)=YOUT(I)          
2000  CONTINUE
      
      !CALL DERIVS(dtheta1+H,u,dydt,N)
      RETURN
      END
         
!C(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)(*)*)(*)(*)(*)(*)(*)
!      SUBROUTINE RK4COEF(H)
!!c      implicit real*8 (A-H,O-Z)
!      double precision A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3
!      COMMON/RK4COEF1/A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3
!      CHARACTER*12 FILE1
!                      
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
