        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:21 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CSATK__genmod
          INTERFACE 
            SUBROUTINE CSATK(ICOMP,T,KPH,P,RHO,CSAT,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: KPH
              REAL(KIND=8) :: P
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: CSAT
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE CSATK
          END INTERFACE 
        END MODULE CSATK__genmod
