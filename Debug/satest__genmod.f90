        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATEST__genmod
          INTERFACE 
            SUBROUTINE SATEST(IFLASH,T,P,X,XLIQ,XVAP,IERR,HERR)
              INTEGER(KIND=4) :: IFLASH
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: XLIQ(20)
              REAL(KIND=8) :: XVAP(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATEST
          END INTERFACE 
        END MODULE SATEST__genmod
