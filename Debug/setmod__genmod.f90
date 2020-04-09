        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETMOD__genmod
          INTERFACE 
            SUBROUTINE SETMOD(NCOMP,HTYPE,HMIX,HCOMP,IERR,HERR)
              INTEGER(KIND=4) :: NCOMP
              CHARACTER(LEN=3) :: HTYPE
              CHARACTER(LEN=3) :: HMIX
              CHARACTER(LEN=3) :: HCOMP(1:20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETMOD
          END INTERFACE 
        END MODULE SETMOD__genmod
