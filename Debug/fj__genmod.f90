        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FJ__genmod
          INTERFACE 
            SUBROUTINE FJ(ICOMP,T,D,F,DFDT,D2FDT2,DFDD,D2FDD2,D2FDTD)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              REAL(KIND=8) :: F
              REAL(KIND=8) :: DFDT
              REAL(KIND=8) :: D2FDT2
              REAL(KIND=8) :: DFDD
              REAL(KIND=8) :: D2FDD2
              REAL(KIND=8) :: D2FDTD
            END SUBROUTINE FJ
          END INTERFACE 
        END MODULE FJ__genmod
