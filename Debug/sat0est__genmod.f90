        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SAT0EST__genmod
          INTERFACE 
            SUBROUTINE SAT0EST(T,P,X,XLIQ,XVAP,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: XLIQ(20)
              REAL(KIND=8) :: XVAP(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SAT0EST
          END INTERFACE 
        END MODULE SAT0EST__genmod
