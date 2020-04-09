        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:37 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ACTVYDLL__genmod
          INTERFACE 
            SUBROUTINE ACTVYDLL(T,RHO,X,ACTV,GAMMA,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: ACTV(20)
              REAL(KIND=8) :: GAMMA(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE ACTVYDLL
          END INTERFACE 
        END MODULE ACTVYDLL__genmod
