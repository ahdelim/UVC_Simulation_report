        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:48 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DBFL1__genmod
          INTERFACE 
            SUBROUTINE DBFL1(D,B,X,AB,T,P,IERR,HERR)
              REAL(KIND=8) :: D
              REAL(KIND=8) :: B
              REAL(KIND=8) :: X(20)
              CHARACTER(LEN=2) :: AB
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE DBFL1
          END INTERFACE 
        END MODULE DBFL1__genmod
