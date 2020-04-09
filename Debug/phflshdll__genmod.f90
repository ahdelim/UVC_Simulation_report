        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:38 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PHFLSHDLL__genmod
          INTERFACE 
            SUBROUTINE PHFLSHDLL(P,H,Z,T,D,DL,DV,X,Y,Q,E,S,CV,CP,W,I,   &
     &HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: H
              REAL(KIND=8) :: Z(20)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              REAL(KIND=8) :: Q
              REAL(KIND=8) :: E
              REAL(KIND=8) :: S
              REAL(KIND=8) :: CV
              REAL(KIND=8) :: CP
              REAL(KIND=8) :: W
              INTEGER(KIND=4) :: I
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE PHFLSHDLL
          END INTERFACE 
        END MODULE PHFLSHDLL__genmod
