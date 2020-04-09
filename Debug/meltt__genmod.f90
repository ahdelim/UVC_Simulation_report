        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:13 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MELTT__genmod
          INTERFACE 
            SUBROUTINE MELTT(T,X,P,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: P
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE MELTT
          END INTERFACE 
        END MODULE MELTT__genmod
