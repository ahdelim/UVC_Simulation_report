        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:56 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PHIK__genmod
          INTERFACE 
            FUNCTION PHIK(ICOMP,ITAU,IDEL,TAU,DEL)
              INTEGER(KIND=4) :: ICOMP
              INTEGER(KIND=4) :: ITAU
              INTEGER(KIND=4) :: IDEL
              REAL(KIND=8) :: TAU
              REAL(KIND=8) :: DEL
              REAL(KIND=8) :: PHIK
            END FUNCTION PHIK
          END INTERFACE 
        END MODULE PHIK__genmod
