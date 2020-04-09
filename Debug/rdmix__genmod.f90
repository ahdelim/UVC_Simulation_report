        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:09 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDMIX__genmod
          INTERFACE 
            SUBROUTINE RDMIX(NREAD,ICOMP,JCOMP,HMODIJ,LIJ,IERR,HERR)
              INTEGER(KIND=4) :: NREAD
              INTEGER(KIND=4) :: ICOMP
              INTEGER(KIND=4) :: JCOMP
              CHARACTER(LEN=3) :: HMODIJ
              LOGICAL(KIND=4) :: LIJ(20,20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE RDMIX
          END INTERFACE 
        END MODULE RDMIX__genmod
