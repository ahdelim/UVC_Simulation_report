        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LIMITXDLL__genmod
          INTERFACE 
            SUBROUTINE LIMITXDLL(HTYP,T,D,P,X,TMIN,TMAX,DMAX,PMAX,IERR, &
     &HERR)
              CHARACTER(LEN=3) :: HTYP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              REAL(KIND=8) :: P
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: TMIN
              REAL(KIND=8) :: TMAX
              REAL(KIND=8) :: DMAX
              REAL(KIND=8) :: PMAX
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE LIMITXDLL
          END INTERFACE 
        END MODULE LIMITXDLL__genmod
