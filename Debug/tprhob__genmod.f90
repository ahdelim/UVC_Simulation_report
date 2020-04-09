        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TPRHOB__genmod
          INTERFACE 
            SUBROUTINE TPRHOB(T,P,RHO1,RHO2,X,RHO,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: RHO1
              REAL(KIND=8) :: RHO2
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: RHO
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TPRHOB
          END INTERFACE 
        END MODULE TPRHOB__genmod
