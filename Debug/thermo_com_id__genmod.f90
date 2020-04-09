        !COMPILER-GENERATED INTERFACE MODULE: Thu Nov 28 15:41:53 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE THERMO_COM_ID__genmod
          INTERFACE 
            SUBROUTINE THERMO_COM_ID(THETA_1,V_SUC,V_COM,P_CSCV_ID,     &
     &P_CDCV_ID)
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
              REAL(KIND=8) :: V_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: V_COM(1:NO_DATA+1)
              REAL(KIND=8) :: P_CSCV_ID(1:NO_DATA+1)
              REAL(KIND=8) :: P_CDCV_ID(1:NO_DATA+1)
            END SUBROUTINE THERMO_COM_ID
          END INTERFACE 
        END MODULE THERMO_COM_ID__genmod
