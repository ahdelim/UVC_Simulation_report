        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:48 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PQFLSH__genmod
          INTERFACE 
            SUBROUTINE PQFLSH(P,Q,Z,KQ,T,D,DL,DV,X,Y,E,H,S,CV,CP,W,IERR,&
     &HERR)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: Q
              REAL(KIND=8) :: Z(20)
              INTEGER(KIND=4) :: KQ
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              REAL(KIND=8) :: E
              REAL(KIND=8) :: H
              REAL(KIND=8) :: S
              REAL(KIND=8) :: CV
              REAL(KIND=8) :: CP
              REAL(KIND=8) :: W
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE PQFLSH
          END INTERFACE 
        END MODULE PQFLSH__genmod