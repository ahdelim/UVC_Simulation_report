        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ETABKP__genmod
          INTERFACE 
            SUBROUTINE ETABKP(JJ,T,RHO,FJ,FX,HJ,HX,ETABK,IERR,HERR)
              INTEGER(KIND=4) :: JJ
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: FJ(20)
              REAL(KIND=8) :: FX
              REAL(KIND=8) :: HJ(20)
              REAL(KIND=8) :: HX
              REAL(KIND=8) :: ETABK
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE ETABKP
          END INTERFACE 
        END MODULE ETABKP__genmod
