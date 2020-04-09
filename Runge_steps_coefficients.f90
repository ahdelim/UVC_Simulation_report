subroutine Runge_steps_coefficients
    include "var_runge_steps_coefficients.f90"
    include "var_simulation_parameter.f90"
    include "var_operational_parameter.f90"
    
    
    time_step = theta_step/omega_1
    
    H_time = time_step
    call RK4COEF_1(H_time)
    
    H_theta = theta_step
    call RK4COEF_2(H_theta)
    
    
end subroutine Runge_steps_coefficients
    
    SUBROUTINE RK4COEF_1(H1)
      !implicit real*8 (A-H,O-Z)
      double precision A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3 
      COMMON/RK4COEF1/A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3
      CHARACTER*12 FILE1
      double precision H1             
      CLOSE(1)
      
      FILE1="Inputs\\RUNGE.RK4"

     !-----------------
     !SET IOPT=-2 AS A DEFAULT FOR THE SECOND CALL TO CHANGE THE
     !COEFF. A1,A2,A3
     !-----------------
                       
      ! PRINT*,' READING NUMERICAL COEFF..'
      OPEN(1,FILE="Inputs\\RUNGE.RK4",STATUS='OLD')
                                     
      READ(1,*)A1
      READ(1,*)A2
      READ(1,*)A3
      READ(1,*)B11
      READ(1,*)B21
      READ(1,*)B22
      READ(1,*)B31
      READ(1,*)B32
      READ(1,*)B33
      READ(1,*)W1
      READ(1,*)W2
      READ(1,*)W3
      READ(1,*)W4
      CLOSE(1)
      X1=H1*A1
      X2=H1*A2
      X3=H1*A3
      
      !IF(IOPT.GT.0)PRINT*,'13 COEFF READ FROM FILE:',FILE1
      RETURN
    END
    
    SUBROUTINE RK4COEF_2(H1)
      !implicit real*8 (A-H,O-Z)
      double precision A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3 
      COMMON/RK4COEF2/A1,A2,A3,B11,B21,B22,B31,B32,B33,W1,W2,W3,W4,X1,X2,X3
      CHARACTER*12 FILE1
      double precision H1              
      CLOSE(1)
      
      FILE1="Inputs\\RUNGE.RK4"

     !-----------------
     !SET IOPT=-2 AS A DEFAULT FOR THE SECOND CALL TO CHANGE THE
     !COEFF. A1,A2,A3
     !-----------------
                       
      ! PRINT*,' READING NUMERICAL COEFF..'
      OPEN(1,FILE="Inputs\\RUNGE.RK4",STATUS='OLD')
                                     
      READ(1,*)A1
      READ(1,*)A2
      READ(1,*)A3
      READ(1,*)B11
      READ(1,*)B21
      READ(1,*)B22
      READ(1,*)B31
      READ(1,*)B32
      READ(1,*)B33
      READ(1,*)W1
      READ(1,*)W2
      READ(1,*)W3
      READ(1,*)W4
      CLOSE(1)
      X1=H1*A1
      X2=H1*A2
      X3=H1*A3
      
      !IF(IOPT.GT.0)PRINT*,'13 COEFF READ FROM FILE:',FILE1
      RETURN
      END