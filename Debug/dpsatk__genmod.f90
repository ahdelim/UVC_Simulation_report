        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:56 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPSATK__genmod
          INTERFACE 
            SUBROUTINE DPSATK(ICOMP,T,DPDT,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: DPDT
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE DPSATK
          END INTERFACE 
        END MODULE DPSATK__genmod
