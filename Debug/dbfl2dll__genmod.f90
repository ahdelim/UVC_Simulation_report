        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DBFL2DLL__genmod
          INTERFACE 
            SUBROUTINE DBFL2DLL(D,B,Z,KQ,AB,T,P,DL,DV,X,Y,Q,IERR,HERR)
              REAL(KIND=8) :: D
              REAL(KIND=8) :: B
              REAL(KIND=8) :: Z(20)
              INTEGER(KIND=4) :: KQ
              CHARACTER(LEN=2) :: AB
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              REAL(KIND=8) :: Q
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE DBFL2DLL
          END INTERFACE 
        END MODULE DBFL2DLL__genmod
