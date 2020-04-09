        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:18 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MINEIGEN__genmod
          INTERFACE 
            SUBROUTINE MINEIGEN(N,A,U,XLAMDA,IERR,HERR)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(20,20)
              REAL(KIND=8) :: U(20)
              REAL(KIND=8) :: XLAMDA
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE MINEIGEN
          END INTERFACE 
        END MODULE MINEIGEN__genmod
