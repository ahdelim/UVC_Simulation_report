        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PTRNEC__genmod
          INTERFACE 
            SUBROUTINE PTRNEC(ICOMP,T,RHO,ETARES,TCXRES,IERR,HERR,IREFN)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: ETARES
              REAL(KIND=8) :: TCXRES
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
              INTEGER(KIND=4) :: IREFN
            END SUBROUTINE PTRNEC
          END INTERFACE 
        END MODULE PTRNEC__genmod
