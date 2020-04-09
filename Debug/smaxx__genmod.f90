        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:21 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SMAXX__genmod
          INTERFACE 
            SUBROUTINE SMAXX(X,SMX,TSMX,SMN,TSMN,SMMIN,TSMMIN,IERR,HERR)
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: SMX
              REAL(KIND=8) :: TSMX
              REAL(KIND=8) :: SMN
              REAL(KIND=8) :: TSMN
              REAL(KIND=8) :: SMMIN
              REAL(KIND=8) :: TSMMIN
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SMAXX
          END INTERFACE 
        END MODULE SMAXX__genmod
