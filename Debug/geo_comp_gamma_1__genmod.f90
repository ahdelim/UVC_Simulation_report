        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:42 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GEO_COMP_GAMMA_1__genmod
          INTERFACE 
            SUBROUTINE GEO_COMP_GAMMA_1(GAMMA_1,L_V,THETA_1,R_OC,R_VH,  &
     &R_RO,E)
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
              REAL(KIND=8) :: GAMMA_1(1:NO_DATA+1)
              REAL(KIND=8) :: L_V(1:NO_DATA+1)
              REAL(KIND=8) :: THETA_1(1:NO_DATA+1)
              REAL(KIND=8) :: R_OC
              REAL(KIND=8) :: R_VH
              REAL(KIND=8) :: R_RO
              REAL(KIND=8) :: E
            END SUBROUTINE GEO_COMP_GAMMA_1
          END INTERFACE 
        END MODULE GEO_COMP_GAMMA_1__genmod
