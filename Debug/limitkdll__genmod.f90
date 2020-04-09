        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LIMITKDLL__genmod
          INTERFACE 
            SUBROUTINE LIMITKDLL(HTYP,ICOMP,T,D,P,TMIN,TMAX,DMAX,PMAX,I,&
     &HERR)
              CHARACTER(LEN=3) :: HTYP
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              REAL(KIND=8) :: P
              REAL(KIND=8) :: TMIN
              REAL(KIND=8) :: TMAX
              REAL(KIND=8) :: DMAX
              REAL(KIND=8) :: PMAX
              INTEGER(KIND=4) :: I
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE LIMITKDLL
          END INTERFACE 
        END MODULE LIMITKDLL__genmod
