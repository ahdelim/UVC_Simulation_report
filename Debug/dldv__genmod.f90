        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DLDV__genmod
          INTERFACE 
            SUBROUTINE DLDV(T,P,RHOL,RHOV,XL,XV,IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: RHOL
              REAL(KIND=8) :: RHOV
              REAL(KIND=8) :: XL(20)
              REAL(KIND=8) :: XV(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE DLDV
          END INTERFACE 
        END MODULE DLDV__genmod
