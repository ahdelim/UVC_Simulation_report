        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:36 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETMIXDLL__genmod
          INTERFACE 
            SUBROUTINE SETMIXDLL(HMXNME,HFMIX,HRF,NCC,HFILE,X,IERR,HERR)
              CHARACTER(LEN=255) :: HMXNME
              CHARACTER(LEN=255) :: HFMIX
              CHARACTER(LEN=3) :: HRF
              INTEGER(KIND=4) :: NCC
              CHARACTER(LEN=10000) :: HFILE
              REAL(KIND=8) :: X(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETMIXDLL
          END INTERFACE 
        END MODULE SETMIXDLL__genmod
