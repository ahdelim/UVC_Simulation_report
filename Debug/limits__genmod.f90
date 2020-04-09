        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:17 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LIMITS__genmod
          INTERFACE 
            SUBROUTINE LIMITS(HTYP,X,TMIN,TMAX,DMAX,PMAX)
              CHARACTER(LEN=3) :: HTYP
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: TMIN
              REAL(KIND=8) :: TMAX
              REAL(KIND=8) :: DMAX
              REAL(KIND=8) :: PMAX
            END SUBROUTINE LIMITS
          END INTERFACE 
        END MODULE LIMITS__genmod
