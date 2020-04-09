        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TRNPRPDLL__genmod
          INTERFACE 
            SUBROUTINE TRNPRPDLL(T,RHO,X,ETA,TCX,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: ETA
              REAL(KIND=8) :: TCX
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TRNPRPDLL
          END INTERFACE 
        END MODULE TRNPRPDLL__genmod
