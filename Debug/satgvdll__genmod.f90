        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:38 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATGVDLL__genmod
          INTERFACE 
            SUBROUTINE SATGVDLL(T,P,Z,VF,B,IPV,ITYP,ISP,RHOX,RHOY,X,Y,  &
     &IERR,HERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: P
              REAL(KIND=8) :: Z(20)
              REAL(KIND=8) :: VF
              REAL(KIND=8) :: B
              INTEGER(KIND=4) :: IPV
              INTEGER(KIND=4) :: ITYP
              INTEGER(KIND=4) :: ISP
              REAL(KIND=8) :: RHOX
              REAL(KIND=8) :: RHOY
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: Y(20)
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE SATGVDLL
          END INTERFACE 
        END MODULE SATGVDLL__genmod