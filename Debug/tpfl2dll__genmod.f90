        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:40 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TPFL2DLL__genmod
          INTERFACE 
            SUBROUTINE TPFL2DLL(T,P,Z,DL,DV,X,Y,Q,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: Z(20)
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              REAL(KIND=8) :: Q
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TPFL2DLL
          END INTERFACE 
        END MODULE TPFL2DLL__genmod
