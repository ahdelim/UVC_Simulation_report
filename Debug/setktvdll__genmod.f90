        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:40 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETKTVDLL__genmod
          INTERFACE 
            SUBROUTINE SETKTVDLL(ICOMP,JCOMP,HMODIJ,FIJ,HFMIX,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              INTEGER(KIND=4) :: JCOMP
              CHARACTER(LEN=3) :: HMODIJ
              REAL(KIND=8) :: FIJ(6)
              CHARACTER(LEN=255) :: HFMIX
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETKTVDLL
          END INTERFACE 
        END MODULE SETKTVDLL__genmod
