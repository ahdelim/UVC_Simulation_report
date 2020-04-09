        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DERIVS_VALVE__genmod
          INTERFACE 
            SUBROUTINE DERIVS_VALVE(DT,G,H,F_MID,INTEGRAL_F,P_CV,P_DISC,&
     &AREA_PORT,RHO_DV,A_CROSS_DV,DPRATIO_DV,G_GRAV,OMEGA_RESPONSE_DV,  &
     &DGT,DHT)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: G
              REAL(KIND=8) :: H
              REAL(KIND=8) :: F_MID
              REAL(KIND=8) :: INTEGRAL_F
              REAL(KIND=8) :: P_CV
              REAL(KIND=8) :: P_DISC
              REAL(KIND=8) :: AREA_PORT
              REAL(KIND=8) :: RHO_DV
              REAL(KIND=8) :: A_CROSS_DV
              REAL(KIND=8) :: DPRATIO_DV
              REAL(KIND=8) :: G_GRAV
              REAL(KIND=8) :: OMEGA_RESPONSE_DV
              REAL(KIND=8) :: DGT
              REAL(KIND=8) :: DHT
            END SUBROUTINE DERIVS_VALVE
          END INTERFACE 
        END MODULE DERIVS_VALVE__genmod
