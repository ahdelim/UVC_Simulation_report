        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PTEST__genmod
          INTERFACE 
            SUBROUTINE PTEST(INP,T,P,ALPHA,X,TC2,PC2,ACF2,CALC,XOUT,IERR&
     &,HERR)
              INTEGER(KIND=4) :: INP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: ALPHA
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: TC2(20)
              REAL(KIND=8) :: PC2(20)
              REAL(KIND=8) :: ACF2(20)
              REAL(KIND=8) :: CALC
              REAL(KIND=8) :: XOUT(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE PTEST
          END INTERFACE 
        END MODULE PTEST__genmod
