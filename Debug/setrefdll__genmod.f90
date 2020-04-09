        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:36 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETREFDLL__genmod
          INTERFACE 
            SUBROUTINE SETREFDLL(HRF,IXFLAG,X0,H0,S0,T0,P0,IERR,HERR)
              CHARACTER(LEN=3) :: HRF
              INTEGER(KIND=4) :: IXFLAG
              REAL(KIND=8) :: X0(20)
              REAL(KIND=8) :: H0
              REAL(KIND=8) :: S0
              REAL(KIND=8) :: T0
              REAL(KIND=8) :: P0
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETREFDLL
          END INTERFACE 
        END MODULE SETREFDLL__genmod
