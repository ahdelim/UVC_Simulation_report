        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:03 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DEK__genmod
          INTERFACE 
            SUBROUTINE DEK(ICOMP,T,RHO,DE,IERR,HERR)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: DE
              INTEGER(KIND=4) :: IERR
              CHARACTER(LEN=255) :: HERR
            END SUBROUTINE DEK
          END INTERFACE 
        END MODULE DEK__genmod
