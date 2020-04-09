        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:38 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TPRHODLL__genmod
          INTERFACE 
            SUBROUTINE TPRHODLL(T,P,X,J,I,RHO,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: I
              REAL(KIND=8) :: RHO
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TPRHODLL
          END INTERFACE 
        END MODULE TPRHODLL__genmod
