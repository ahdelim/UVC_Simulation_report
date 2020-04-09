        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:03 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TCXKB__genmod
          INTERFACE 
            SUBROUTINE TCXKB(ICOMP,T,RHO,TCXB,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: TCXB
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE TCXKB
          END INTERFACE 
        END MODULE TCXKB__genmod
