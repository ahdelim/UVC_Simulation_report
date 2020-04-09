        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:25 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE KINE_VANE__genmod
          INTERFACE 
            SUBROUTINE KINE_VANE(DLVDT,DLVDTT,DLVDTHETA1,DLVDTHETA12,   &
     &THETA_1)
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
              REAL(KIND=8) :: DLVDT(1:NO_DATA+1)
              REAL(KIND=8) :: DLVDTT(1:NO_DATA+1)
              REAL(KIND=8) :: DLVDTHETA1(1:NO_DATA+1)
              REAL(KIND=8) :: DLVDTHETA12(1:NO_DATA+1)
              REAL(KIND=8) :: THETA_1(1:NO_DATA+1)
            END SUBROUTINE KINE_VANE
          END INTERFACE 
        END MODULE KINE_VANE__genmod
