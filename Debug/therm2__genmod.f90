        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE THERM2__genmod
          INTERFACE 
            SUBROUTINE THERM2(T,RHO,X,P,E,H,S,CV,CP,W,Z,HJT,A,G,XKAPPA, &
     &BETA,DPDRHO,D2PDD2,DPT,DRHODT,DRHODP,D2PT2,D2PDTD,SPARE3,SPARE4)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: E
              REAL(KIND=8) :: H
              REAL(KIND=8) :: S
              REAL(KIND=8) :: CV
              REAL(KIND=8) :: CP
              REAL(KIND=8) :: W
              REAL(KIND=8) :: Z
              REAL(KIND=8) :: HJT
              REAL(KIND=8) :: A
              REAL(KIND=8) :: G
              REAL(KIND=8) :: XKAPPA
              REAL(KIND=8) :: BETA
              REAL(KIND=8) :: DPDRHO
              REAL(KIND=8) :: D2PDD2
              REAL(KIND=8) :: DPT
              REAL(KIND=8) :: DRHODT
              REAL(KIND=8) :: DRHODP
              REAL(KIND=8) :: D2PT2
              REAL(KIND=8) :: D2PDTD
              REAL(KIND=8) :: SPARE3
              REAL(KIND=8) :: SPARE4
            END SUBROUTINE THERM2
          END INTERFACE 
        END MODULE THERM2__genmod
