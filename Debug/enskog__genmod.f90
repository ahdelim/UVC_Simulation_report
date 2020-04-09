        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ENSKOG__genmod
          INTERFACE 
            SUBROUTINE ENSKOG(N,RHO,SIGM,CMW,X,ETA)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: SIGM(20,20)
              REAL(KIND=8) :: CMW(20)
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: ETA
            END SUBROUTINE ENSKOG
          END INTERFACE 
        END MODULE ENSKOG__genmod
