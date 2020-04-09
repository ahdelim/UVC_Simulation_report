        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:38 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATDDLL__genmod
          INTERFACE 
            SUBROUTINE SATDDLL(RHO,X,KPH,KR,T,P,RHOL,RHOV,XLIQ,XVAP,I,  &
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
              INTEGER(KIND=4) :: I
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATDDLL
          END INTERFACE 
        END MODULE SATDDLL__genmod
