        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DHD1__genmod
          INTERFACE 
            SUBROUTINE DHD1(T,RHO,X,DHDT_D,DHDT_P,DHDD_T,DHDD_P,DHDP_T, &
     &DHDP_D)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: DHDT_D
              REAL(KIND=8) :: DHDT_P
              REAL(KIND=8) :: DHDD_T
              REAL(KIND=8) :: DHDD_P
              REAL(KIND=8) :: DHDP_T
              REAL(KIND=8) :: DHDP_D
            END SUBROUTINE DHD1
          END INTERFACE 
        END MODULE DHD1__genmod
