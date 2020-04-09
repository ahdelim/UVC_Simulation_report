        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:48 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PQFL2__genmod
          INTERFACE 
            SUBROUTINE PQFL2(P,Q,Z,KQ,KSAT,TBUB,TDEW,DLBUB,DVDEW,YBUB,  &
     &XDEW,T,DL,DV,X,Y,IERR,HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: Q
              REAL(KIND=8) :: Z(20)
              INTEGER(KIND=4) :: KQ
              INTEGER(KIND=4) :: KSAT
              REAL(KIND=8) :: TBUB
              REAL(KIND=8) :: TDEW
              REAL(KIND=8) :: DLBUB
              REAL(KIND=8) :: DVDEW
              REAL(KIND=8) :: YBUB(20)
              REAL(KIND=8) :: XDEW(20)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE PQFL2
          END INTERFACE 
        END MODULE PQFL2__genmod
