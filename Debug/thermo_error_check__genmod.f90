        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:44 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE THERMO_ERROR_CHECK__genmod
          INTERFACE 
            SUBROUTINE THERMO_ERROR_CHECK(K,K_STOP,P_SCV,P_DCV,T_SCV,   &
     &T_DCV,P_TEST_SUCT,P_TEST_COMP,T_TEST_SUCT,T_TEST_COMP)
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
              INTEGER(KIND=4) :: K_STOP
              INTEGER(KIND=4) :: K
              REAL(KIND=8) :: P_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_TEST_SUCT(1:K_STOP)
              REAL(KIND=8) :: P_TEST_COMP(1:K_STOP)
              REAL(KIND=8) :: T_TEST_SUCT(1:K_STOP)
              REAL(KIND=8) :: T_TEST_COMP(1:K_STOP)
            END SUBROUTINE THERMO_ERROR_CHECK
          END INTERFACE 
        END MODULE THERMO_ERROR_CHECK__genmod
