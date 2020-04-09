        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:00 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TPRHO__genmod
          INTERFACE 
            SUBROUTINE TPRHO(T,P,X,KPH,KGUESS,RHO,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              INTEGER(KIND=4) :: KGUESS
              REAL(KIND=8) :: RHO
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TPRHO
          END INTERFACE 
        END MODULE TPRHO__genmod
