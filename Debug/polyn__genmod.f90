        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:17 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE POLYN__genmod
          INTERFACE 
            SUBROUTINE POLYN(N,XPNTS,YPNTS,XVAL,YVAL,IERR,HERR)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: XPNTS(*)
              REAL(KIND=8) :: YPNTS(*)
              REAL(KIND=8) :: XVAL
              REAL(KIND=8) :: YVAL
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE POLYN
          END INTERFACE 
        END MODULE POLYN__genmod
