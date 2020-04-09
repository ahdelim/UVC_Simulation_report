        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE EXCESS__genmod
          INTERFACE 
            SUBROUTINE EXCESS(T,P,X,KPH,RHO,VE,EE,HE,SE,AE,GE,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: VE
              REAL(KIND=8) :: EE
              REAL(KIND=8) :: HE
              REAL(KIND=8) :: SE
              REAL(KIND=8) :: AE
              REAL(KIND=8) :: GE
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE EXCESS
          END INTERFACE 
        END MODULE EXCESS__genmod
