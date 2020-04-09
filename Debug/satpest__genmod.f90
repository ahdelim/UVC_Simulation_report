        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATPEST__genmod
          INTERFACE 
            SUBROUTINE SATPEST(P,X,KPH,T,X2,IERR,HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              REAL(KIND=8) :: T
              REAL(KIND=8) :: X2(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATPEST
          END INTERFACE 
        END MODULE SATPEST__genmod
