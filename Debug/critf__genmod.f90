        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:42 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CRITF__genmod
          INTERFACE 
            SUBROUTINE CRITF(ZETA,X,TC,PC,DC,IERR,HERR)
              REAL(KIND=8) :: ZETA(20)
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: TC
              REAL(KIND=8) :: PC
              REAL(KIND=8) :: DC
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE CRITF
          END INTERFACE 
        END MODULE CRITF__genmod
