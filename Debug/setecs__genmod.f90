        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETECS__genmod
          INTERFACE 
            SUBROUTINE SETECS(NREAD,ICOMP,HCASNO,HREFF,HEQN,IERR,HERR)
              INTEGER(KIND=4) :: NREAD
              INTEGER(KIND=4) :: ICOMP
              CHARACTER(LEN=12) :: HCASNO
              CHARACTER(LEN=255) :: HREFF
              CHARACTER(LEN=3) :: HEQN
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETECS
          END INTERFACE 
        END MODULE SETECS__genmod
