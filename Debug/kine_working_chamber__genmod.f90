        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:00 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE KINE_WORKING_CHAMBER__genmod
          INTERFACE 
            SUBROUTINE KINE_WORKING_CHAMBER(THETA_1,DLVDTHETA1,DTHETA2DT&
     &,DVDTHETA1_SUC,DVDTHETA1_COM)
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
              REAL(KIND=8) :: THETA_1(1:NO_DATA+1)
              REAL(KIND=8) :: DLVDTHETA1(1:NO_DATA+1)
              REAL(KIND=8) :: DTHETA2DT(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_COM(1:NO_DATA+1)
            END SUBROUTINE KINE_WORKING_CHAMBER
          END INTERFACE 
        END MODULE KINE_WORKING_CHAMBER__genmod
