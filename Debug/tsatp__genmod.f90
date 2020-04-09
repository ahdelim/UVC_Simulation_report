        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:57 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TSATP__genmod
          INTERFACE 
            SUBROUTINE TSATP(P,X,T,IERR,HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TSATP
          END INTERFACE 
        END MODULE TSATP__genmod
