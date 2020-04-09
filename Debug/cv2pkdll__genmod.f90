        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:38 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CV2PKDLL__genmod
          INTERFACE 
            SUBROUTINE CV2PKDLL(ICOMP,T,RHO,CV2P,CSAT,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: CV2P
              REAL(KIND=8) :: CSAT
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE CV2PKDLL
          END INTERFACE 
        END MODULE CV2PKDLL__genmod
