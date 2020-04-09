        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:17 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MINP__genmod
          INTERFACE 
            SUBROUTINE MINP(N,XPNTS,YPNTS,XVAL,IERR,HERR)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: XPNTS(3)
              REAL(KIND=8) :: YPNTS(3)
              REAL(KIND=8) :: XVAL
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE MINP
          END INTERFACE 
        END MODULE MINP__genmod
