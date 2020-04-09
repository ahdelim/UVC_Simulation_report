        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:48 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ABFL2__genmod
          INTERFACE 
            SUBROUTINE ABFL2(A,B,Z,KQ,KSAT,AB,TBUB,TDEW,PBUB,PDEW,DLBUB,&
     &DVDEW,YBUB,XDEW,T,P,DL,DV,X,Y,Q,IERR,HERR)
              REAL(KIND=8) :: A
              REAL(KIND=8) :: B
              REAL(KIND=8) :: Z(20)
              INTEGER(KIND=4) :: KQ
              INTEGER(KIND=4) :: KSAT
              CHARACTER(LEN=2) :: AB
              REAL(KIND=8) :: TBUB
              REAL(KIND=8) :: TDEW
              REAL(KIND=8) :: PBUB
              REAL(KIND=8) :: PDEW
              REAL(KIND=8) :: DLBUB
              REAL(KIND=8) :: DVDEW
              REAL(KIND=8) :: YBUB(20)
              REAL(KIND=8) :: XDEW(20)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              REAL(KIND=8) :: Q
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE ABFL2
          END INTERFACE 
        END MODULE ABFL2__genmod
