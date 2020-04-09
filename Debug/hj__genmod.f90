        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HJ__genmod
          INTERFACE 
            SUBROUTINE HJ(ICOMP,T,D,H,DHDT,D2HDT2,DHDD,D2HDD2,D2HDTD)
              INTEGER(KIND=4) :: ICOMP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              REAL(KIND=8) :: H
              REAL(KIND=8) :: DHDT
              REAL(KIND=8) :: D2HDT2
              REAL(KIND=8) :: DHDD
              REAL(KIND=8) :: D2HDD2
              REAL(KIND=8) :: D2HDTD
            END SUBROUTINE HJ
          END INTERFACE 
        END MODULE HJ__genmod
