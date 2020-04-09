        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:46 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETMIX__genmod
          INTERFACE 
            SUBROUTINE SETMIX(HMXNME,HFMIX,HRF,NCC,HFILES,X,IERR,HERR)
              CHARACTER(LEN=255) :: HMXNME
              CHARACTER(LEN=255) :: HFMIX
              CHARACTER(LEN=3) :: HRF
              INTEGER(KIND=4) :: NCC
              CHARACTER(LEN=255) :: HFILES(20)
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETMIX
          END INTERFACE 
        END MODULE SETMIX__genmod
