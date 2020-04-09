        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SURTENDLL__genmod
          INTERFACE 
            SUBROUTINE SURTENDLL(T,RHOL,RHOV,XL,XV,SIGMA,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHOL
              REAL(KIND=8) :: RHOV
              REAL(KIND=8) :: XL(20)
              REAL(KIND=8) :: XV(20)
              REAL(KIND=8) :: SIGMA
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SURTENDLL
          END INTERFACE 
        END MODULE SURTENDLL__genmod
