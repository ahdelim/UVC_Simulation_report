        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:13 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SOLVEM__genmod
          INTERFACE 
            SUBROUTINE SOLVEM(IFLAG,P,X,T,IERR,HERR)
              INTEGER(KIND=4) :: IFLAG
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SOLVEM
          END INTERFACE 
        END MODULE SOLVEM__genmod
