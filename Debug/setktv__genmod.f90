        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:59 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETKTV__genmod
          INTERFACE 
            SUBROUTINE SETKTV(ICOMP,JCOMP,HMODIJ,FIJ,HFMIX,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              INTEGER(KIND=4) :: JCOMP
              CHARACTER(LEN=3) :: HMODIJ
              REAL(KIND=8) :: FIJ(6)
              CHARACTER(LEN=255) :: HFMIX
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETKTV
          END INTERFACE 
        END MODULE SETKTV__genmod
