        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PHIDERV__genmod
          INTERFACE 
            SUBROUTINE PHIDERV(IDERV,T,RHO,X,DADN,DNADN,IERR,HERR)
              INTEGER(KIND=4) :: IDERV
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: DADN(20)
              REAL(KIND=8) :: DNADN(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE PHIDERV
          END INTERFACE 
        END MODULE PHIDERV__genmod
