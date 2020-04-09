        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SURFTDLL__genmod
          INTERFACE 
            SUBROUTINE SURFTDLL(T,RHO,X,SIGMA,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: SIGMA
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SURFTDLL
          END INTERFACE 
        END MODULE SURFTDLL__genmod
