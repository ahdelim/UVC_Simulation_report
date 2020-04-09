        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETUP__genmod
          INTERFACE 
            SUBROUTINE SETUP(NCOMP,HFILES,HFMIX,HRF,IERR,HERR)
              INTEGER(KIND=4) :: NCOMP
              CHARACTER(LEN=255) :: HFILES(20)
              CHARACTER(LEN=255) :: HFMIX
              CHARACTER(LEN=3) :: HRF
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETUP
          END INTERFACE 
        END MODULE SETUP__genmod
