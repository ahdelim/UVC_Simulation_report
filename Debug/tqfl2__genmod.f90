        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:48 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TQFL2__genmod
          INTERFACE 
            SUBROUTINE TQFL2(T,Q,Z,KQ,KSAT,PBUB,PDEW,DLBUB,DVDEW,YBUB,  &
     &XDEW,P,DL,DV,X,Y,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: Q
              REAL(KIND=8) :: Z(20)
              INTEGER(KIND=4) :: KQ
              INTEGER(KIND=4) :: KSAT
              REAL(KIND=8) :: PBUB
              REAL(KIND=8) :: PDEW
              REAL(KIND=8) :: DLBUB
              REAL(KIND=8) :: DVDEW
              REAL(KIND=8) :: YBUB(20)
              REAL(KIND=8) :: XDEW(20)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TQFL2
          END INTERFACE 
        END MODULE TQFL2__genmod
