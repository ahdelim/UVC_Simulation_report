        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:18 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CSPLNVAL__genmod
          INTERFACE 
            SUBROUTINE CSPLNVAL(N,XI,FI,COEFF,X,F,IERR,HERR)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: XI(1000)
              REAL(KIND=8) :: FI(1000)
              REAL(KIND=8) :: COEFF(1000)
              REAL(KIND=8) :: X
              REAL(KIND=8) :: F
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE CSPLNVAL
          END INTERFACE 
        END MODULE CSPLNVAL__genmod
