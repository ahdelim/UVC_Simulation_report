        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LIMITSDLL__genmod
          INTERFACE 
            SUBROUTINE LIMITSDLL(HTYP,X,TMIN,TMAX,DMAX,PMAX)
              CHARACTER(LEN=3) :: HTYP
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: TMIN
              REAL(KIND=8) :: TMAX
              REAL(KIND=8) :: DMAX
              REAL(KIND=8) :: PMAX
            END SUBROUTINE LIMITSDLL
          END INTERFACE 
        END MODULE LIMITSDLL__genmod
