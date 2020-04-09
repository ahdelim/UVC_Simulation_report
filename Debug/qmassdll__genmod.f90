        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE QMASSDLL__genmod
          INTERFACE 
            SUBROUTINE QMASSDLL(QMOL,XL,XV,QKG,XLKG,XVKG,WLIQ,WVAP,IERR,&
     &HERR)
              REAL(KIND=8) :: QMOL
              REAL(KIND=8) :: XL(20)
              REAL(KIND=8) :: XV(20)
              REAL(KIND=8) :: QKG
              REAL(KIND=8) :: XLKG(20)
              REAL(KIND=8) :: XVKG(20)
              REAL(KIND=8) :: WLIQ
              REAL(KIND=8) :: WVAP
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE QMASSDLL
          END INTERFACE 
        END MODULE QMASSDLL__genmod
