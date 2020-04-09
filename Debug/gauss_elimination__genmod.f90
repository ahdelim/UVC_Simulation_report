        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:04 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GAUSS_ELIMINATION__genmod
          INTERFACE 
            SUBROUTINE GAUSS_ELIMINATION(A,B,X,N)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(N,N)
              REAL(KIND=8) :: B(N)
              REAL(KIND=8) :: X(N)
            END SUBROUTINE GAUSS_ELIMINATION
          END INTERFACE 
        END MODULE GAUSS_ELIMINATION__genmod
