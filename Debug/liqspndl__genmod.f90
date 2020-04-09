        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LIQSPNDL__genmod
          INTERFACE 
            SUBROUTINE LIQSPNDL(T,X,RHOL,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: RHOL
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE LIQSPNDL
          END INTERFACE 
        END MODULE LIQSPNDL__genmod
