        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TCBKMX__genmod
          INTERFACE 
            SUBROUTINE TCBKMX(T,RHO,X,FJ,FX,HJ,HX,TCX,FLAM,LERRT,LERRD, &
     &IERR,HERR,IREFN)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: FJ(20)
              REAL(KIND=8) :: FX
              REAL(KIND=8) :: HJ(20)
              REAL(KIND=8) :: HX
              REAL(KIND=8) :: TCX
              REAL(KIND=8) :: FLAM
              LOGICAL(KIND=4) :: LERRT
              LOGICAL(KIND=4) :: LERRD
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
              INTEGER(KIND=4) :: IREFN
            END SUBROUTINE TCBKMX
          END INTERFACE 
        END MODULE TCBKMX__genmod
