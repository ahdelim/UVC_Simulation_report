        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:50 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OPTIMIZATION_INDICATED_WORK__genmod
          INTERFACE 
            SUBROUTINE OPTIMIZATION_INDICATED_WORK(V_SUC,V_COM,P_CSCV_ID&
     &,P_CDCV_ID,P_SCV,P_DCV,DVDTHETA1_S,DVDTHETA1_D,POWER_IND,         &
     &POWER_IND_IDEAL,POWER_LOSS_SUC,POWER_LOSS_DISC,POWER_COMP_ONLY)
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
              REAL(KIND=8) :: V_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: V_COM(1:NO_DATA+1)
              REAL(KIND=8) :: P_CSCV_ID(1:NO_DATA+1)
              REAL(KIND=8) :: P_CDCV_ID(1:NO_DATA+1)
              REAL(KIND=8) :: P_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_S(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_D(1:NO_DATA+1)
              REAL(KIND=8) :: POWER_IND
              REAL(KIND=8) :: POWER_IND_IDEAL
              REAL(KIND=8) :: POWER_LOSS_SUC
              REAL(KIND=8) :: POWER_LOSS_DISC
              REAL(KIND=8) :: POWER_COMP_ONLY
            END SUBROUTINE OPTIMIZATION_INDICATED_WORK
          END INTERFACE 
        END MODULE OPTIMIZATION_INDICATED_WORK__genmod
