        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:59 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE STFLD2__genmod
          INTERFACE 
            SUBROUTINE STFLD2(NREAD,I,HCASN,HCITE,HEQN,HETEMP,HFILE,    &
     &HFLAG,HREFF,HSTAR,HTTEMP,HTYPE,LETA,LTCX,IERR,HERR)
              INTEGER(KIND=4) :: NREAD
              INTEGER(KIND=4) :: I
              CHARACTER(LEN=12) :: HCASN(-30:20)
              CHARACTER(LEN=251) :: HCITE
              CHARACTER(LEN=3) :: HEQN
              CHARACTER(LEN=3) :: HETEMP
              CHARACTER(LEN=255) :: HFILE(-30:20)
              CHARACTER(LEN=3) :: HFLAG
              CHARACTER(LEN=255) :: HREFF
              CHARACTER(LEN=1) :: HSTAR
              CHARACTER(LEN=3) :: HTTEMP
              CHARACTER(LEN=3) :: HTYPE
              LOGICAL(KIND=4) :: LETA
              LOGICAL(KIND=4) :: LTCX
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE STFLD2
          END INTERFACE 
        END MODULE STFLD2__genmod
