        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:03 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ETAK__genmod
          INTERFACE 
            SUBROUTINE ETAK(ICOMP,T,RHO,ETA,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: ETA
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE ETAK
          END INTERFACE 
        END MODULE ETAK__genmod
