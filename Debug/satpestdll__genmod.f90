        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:38 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATPESTDLL__genmod
          INTERFACE 
            SUBROUTINE SATPESTDLL(P,X,KPH,T,X2,IERR,HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              REAL(KIND=8) :: T
              REAL(KIND=8) :: X2(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATPESTDLL
          END INTERFACE 
        END MODULE SATPESTDLL__genmod
