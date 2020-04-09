        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATD__genmod
          INTERFACE 
            SUBROUTINE SATD(RHO,X,KPH,KR,T,P,RHOL,RHOV,XLIQ,XVAP,IERR,  &
     &HERR)
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              INTEGER(KIND=4) :: KR
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: RHOL
              REAL(KIND=8) :: RHOV
              REAL(KIND=8) :: XLIQ(20)
              REAL(KIND=8) :: XVAP(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATD
          END INTERFACE 
        END MODULE SATD__genmod
