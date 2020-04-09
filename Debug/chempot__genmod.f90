        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHEMPOT__genmod
          INTERFACE 
            SUBROUTINE CHEMPOT(T,RHO,X,U,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: U(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE CHEMPOT
          END INTERFACE 
        END MODULE CHEMPOT__genmod
