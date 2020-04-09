        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATE__genmod
          INTERFACE 
            SUBROUTINE SATE(E,X,KPH,NROOT,K1,T1,P1,D1,K2,T2,P2,D2,IERR, &
     &HERR)
              REAL(KIND=8) :: E
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: KPH
              INTEGER(KIND=4) :: NROOT
              INTEGER(KIND=4) :: K1
              REAL(KIND=8) :: T1
              REAL(KIND=8) :: P1
              REAL(KIND=8) :: D1
              INTEGER(KIND=4) :: K2
              REAL(KIND=8) :: T2
              REAL(KIND=8) :: P2
              REAL(KIND=8) :: D2
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATE
          END INTERFACE 
        END MODULE SATE__genmod
