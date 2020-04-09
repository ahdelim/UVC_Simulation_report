        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:17 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LUDECOMP__genmod
          INTERFACE 
            SUBROUTINE LUDECOMP(N,MAXN,AMATRIX,CMATRIX,IERR,HERR)
              INTEGER(KIND=4) :: MAXN
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: AMATRIX(MAXN,MAXN)
              REAL(KIND=8) :: CMATRIX(MAXN)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE LUDECOMP
          END INTERFACE 
        END MODULE LUDECOMP__genmod
