        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:56 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PHIX__genmod
          INTERFACE 
            FUNCTION PHIX(ITAU,IDEL,TAU,DEL,X)
              INTEGER(KIND=4) :: ITAU
              INTEGER(KIND=4) :: IDEL
              REAL(KIND=8) :: TAU
              REAL(KIND=8) :: DEL
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: PHIX
            END FUNCTION PHIX
          END INTERFACE 
        END MODULE PHIX__genmod
