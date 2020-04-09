        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ABFL1DLL__genmod
          INTERFACE 
            SUBROUTINE ABFL1DLL(A,B,X,KPH,AB,DMIN,DMAX,T,P,D,IERR,HERR)
              REAL(KIND=8) :: A
              REAL(KIND=8) :: B
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              CHARACTER(LEN=2) :: AB
              REAL(KIND=8) :: DMIN
              REAL(KIND=8) :: DMAX
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: D
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE ABFL1DLL
          END INTERFACE 
        END MODULE ABFL1DLL__genmod
