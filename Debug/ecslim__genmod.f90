        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ECSLIM__genmod
          INTERFACE 
            SUBROUTINE ECSLIM(T,D,TMIN,TMAX,DMAX,LERRT,LERRD,TERR,DERR)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: D
              REAL(KIND=8) :: TMIN
              REAL(KIND=8) :: TMAX
              REAL(KIND=8) :: DMAX
              LOGICAL(KIND=4) :: LERRT
              LOGICAL(KIND=4) :: LERRD
              REAL(KIND=8) :: TERR
              REAL(KIND=8) :: DERR
            END SUBROUTINE ECSLIM
          END INTERFACE 
        END MODULE ECSLIM__genmod
