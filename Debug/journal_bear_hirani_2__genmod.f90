        !COMPILER-GENERATED INTERFACE MODULE: Thu Nov 28 16:12:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE JOURNAL_BEAR_HIRANI_2__genmod
          INTERFACE 
            SUBROUTINE JOURNAL_BEAR_HIRANI_2(THETA_1,DGAMMADT_1,F_CX,   &
     &F_CY,F_RESULTANT,ECCR,ATT,H_MIN,P_MAX,Q_MARTIN,CL_ROTOR_RAD_DY)
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
              REAL(KIND=8) :: DGAMMADT_1(1:NO_DATA+1)
              REAL(KIND=8) :: F_CX(1:NO_DATA+1)
              REAL(KIND=8) :: F_CY(1:NO_DATA+1)
              REAL(KIND=8) :: F_RESULTANT(1:NO_DATA+1)
              REAL(KIND=8) :: ECCR(1:NO_DATA+1)
              REAL(KIND=8) :: ATT(1:NO_DATA+1)
              REAL(KIND=8) :: H_MIN(1:NO_DATA+1)
              REAL(KIND=8) :: P_MAX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_MARTIN(1:NO_DATA+1)
              REAL(KIND=8) :: CL_ROTOR_RAD_DY(1:NO_DATA+1)
            END SUBROUTINE JOURNAL_BEAR_HIRANI_2
          END INTERFACE 
        END MODULE JOURNAL_BEAR_HIRANI_2__genmod
