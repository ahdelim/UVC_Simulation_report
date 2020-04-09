        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:50 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OPTIMIZATION_THERMO_MAIN__genmod
          INTERFACE 
            SUBROUTINE OPTIMIZATION_THERMO_MAIN
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
            END SUBROUTINE OPTIMIZATION_THERMO_MAIN
          END INTERFACE 
        END MODULE OPTIMIZATION_THERMO_MAIN__genmod
