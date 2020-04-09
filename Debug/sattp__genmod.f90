        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:21 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATTP__genmod
          INTERFACE 
            SUBROUTINE SATTP(T,P,X,IFLSH,IGUESS,D,DL,DV,XLIQ,XVAP,Q,IERR&
     &,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: IFLSH
              INTEGER(KIND=4) :: IGUESS
              REAL(KIND=8) :: D
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              REAL(KIND=8) :: XLIQ(20)
              REAL(KIND=8) :: XVAP(20)
              REAL(KIND=8) :: Q
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATTP
          END INTERFACE 
        END MODULE SATTP__genmod
