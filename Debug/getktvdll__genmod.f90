        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:40 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GETKTVDLL__genmod
          INTERFACE 
            SUBROUTINE GETKTVDLL(ICOMP,JCOMP,HMODIJ,FIJ,HFMIX,HFIJ2,    &
     &HBINP,HMXRUL)
              INTEGER(KIND=4) :: ICOMP
              INTEGER(KIND=4) :: JCOMP
              CHARACTER(LEN=3) :: HMODIJ
              REAL(KIND=8) :: FIJ(6)
              CHARACTER(LEN=255) :: HFMIX
              CHARACTER(LEN=255) :: HFIJ2
              CHARACTER(LEN=255) :: HBINP
              CHARACTER(LEN=255) :: HMXRUL
            END SUBROUTINE GETKTVDLL
          END INTERFACE 
        END MODULE GETKTVDLL__genmod
