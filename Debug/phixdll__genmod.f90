        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:41 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PHIXDLL__genmod
          INTERFACE 
            SUBROUTINE PHIXDLL(ITAU,IDEL,TAU,DEL,X,PHIXX)
              INTEGER(KIND=4) :: ITAU
              INTEGER(KIND=4) :: IDEL
              REAL(KIND=8) :: TAU
              REAL(KIND=8) :: DEL
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: PHIXX
            END SUBROUTINE PHIXDLL
          END INTERFACE 
        END MODULE PHIXDLL__genmod
