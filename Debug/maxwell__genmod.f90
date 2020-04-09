        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MAXWELL__genmod
          INTERFACE 
            SUBROUTINE MAXWELL(ICOMP,TS,PS,DL,DV,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: TS
              REAL(KIND=8) :: PS
              REAL(KIND=8) :: DL
              REAL(KIND=8) :: DV
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE MAXWELL
          END INTERFACE 
        END MODULE MAXWELL__genmod
