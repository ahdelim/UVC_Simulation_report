        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:38 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATPDLL__genmod
          INTERFACE 
            SUBROUTINE SATPDLL(P,X,KPH,T,RHOL,RHOV,XLIQ,XVAP,IERR,HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHOL
              REAL(KIND=8) :: RHOV
              REAL(KIND=8) :: XLIQ(20)
              REAL(KIND=8) :: XVAP(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATPDLL
          END INTERFACE 
        END MODULE SATPDLL__genmod
