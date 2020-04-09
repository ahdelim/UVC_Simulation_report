        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:59 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GETKTV__genmod
          INTERFACE 
            SUBROUTINE GETKTV(ICOMP,JCOMP,HMODIJ,FIJ,HFMIX,HFIJ,HBINP,  &
     &HMXRUL)
              INTEGER(KIND=4) :: ICOMP
              INTEGER(KIND=4) :: JCOMP
              CHARACTER(LEN=3) :: HMODIJ
              REAL(KIND=8) :: FIJ(6)
              CHARACTER(LEN=255) :: HFMIX
              CHARACTER(LEN=8) :: HFIJ(6)
              CHARACTER(LEN=255) :: HBINP
              CHARACTER(LEN=255) :: HMXRUL
            END SUBROUTINE GETKTV
          END INTERFACE 
        END MODULE GETKTV__genmod
