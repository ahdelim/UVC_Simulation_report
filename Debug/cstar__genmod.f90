        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:49 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CSTAR__genmod
          INTERFACE 
            SUBROUTINE CSTAR(T,P,V,X,CS,TS,DS,PS,WS,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: V
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: CS
              REAL(KIND=8) :: TS
              REAL(KIND=8) :: DS
              REAL(KIND=8) :: PS
              REAL(KIND=8) :: WS
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE CSTAR
          END INTERFACE 
        END MODULE CSTAR__genmod
