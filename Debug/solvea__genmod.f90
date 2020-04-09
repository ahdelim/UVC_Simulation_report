        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:57 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SOLVEA__genmod
          INTERFACE 
            SUBROUTINE SOLVEA(IFLAG,PD,X,T,IERR,HERR)
              INTEGER(KIND=4) :: IFLAG
              REAL(KIND=8) :: PD
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SOLVEA
          END INTERFACE 
        END MODULE SOLVEA__genmod
