        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:00 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PHFL1__genmod
          INTERFACE 
            SUBROUTINE PHFL1(P,H,X,KPH,T,D,IERR,HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: H
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE PHFL1
          END INTERFACE 
        END MODULE PHFL1__genmod
