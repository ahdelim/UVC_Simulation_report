        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:30 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MAXT__genmod
          INTERFACE 
            SUBROUTINE MAXT(X,TM,PM,DM,IERR,HERR)
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: TM
              REAL(KIND=8) :: PM
              REAL(KIND=8) :: DM
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE MAXT
          END INTERFACE 
        END MODULE MAXT__genmod
