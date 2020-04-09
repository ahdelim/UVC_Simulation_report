        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DQFL2DLL__genmod
          INTERFACE 
            SUBROUTINE DQFL2DLL(D,Q,Z,KQ,T,P,DL,DV,X,Y,IERR,HERR)
              REAL(KIND=8) :: D
              REAL(KIND=8) :: Q
              REAL(KIND=8) :: Z(20)
              INTEGER(KIND=4) :: KQ
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE DQFL2DLL
          END INTERFACE 
        END MODULE DQFL2DLL__genmod
