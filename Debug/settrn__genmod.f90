        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SETTRN__genmod
          INTERFACE 
            SUBROUTINE SETTRN(NREAD,ICOMP,HCASNO,HREFF,HEOSS,HVS,HTC,   &
     &IERR,HERR)
              INTEGER(KIND=4) :: NREAD
              INTEGER(KIND=4) :: ICOMP
              CHARACTER(LEN=12) :: HCASNO
              CHARACTER(LEN=255) :: HREFF
              CHARACTER(LEN=3) :: HEOSS
              CHARACTER(LEN=3) :: HVS
              CHARACTER(LEN=3) :: HTC
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SETTRN
          END INTERFACE 
        END MODULE SETTRN__genmod
