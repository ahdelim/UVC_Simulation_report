        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HEAT__genmod
          INTERFACE 
            SUBROUTINE HEAT(T,RHO,X,HG,HN,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: HG
              REAL(KIND=8) :: HN
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE HEAT
          END INTERFACE 
        END MODULE HEAT__genmod
