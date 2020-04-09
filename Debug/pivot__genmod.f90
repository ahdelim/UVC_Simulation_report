        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:17 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PIVOT__genmod
          INTERFACE 
            SUBROUTINE PIVOT(N,MAXN,J,IORD,AMATRIX,SDECOMP)
              INTEGER(KIND=4) :: MAXN
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: J
              INTEGER(KIND=4) :: IORD(100)
              REAL(KIND=8) :: AMATRIX(MAXN,MAXN)
              REAL(KIND=8) :: SDECOMP(MAXN)
            END SUBROUTINE PIVOT
          END INTERFACE 
        END MODULE PIVOT__genmod
