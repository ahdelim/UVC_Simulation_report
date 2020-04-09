        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RK4_VALVE__genmod
          INTERFACE 
            SUBROUTINE RK4_VALVE(G,H,G_R,H_R,DGDT,N,DHDT,F_MID,         &
     &INTEGRAL_F,P_CV,P_DISC,AREA_PORT,RHO_DV,A_CROSS_DV,DPRATIO_DV,    &
     &G_GRAV,OMEGA_RESPONSE_DV,YOUT,GN,HN)
              REAL(KIND=8) :: G
              REAL(KIND=8) :: H
              REAL(KIND=8) :: G_R
              REAL(KIND=8) :: H_R
              REAL(KIND=8) :: DGDT
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: DHDT
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
              REAL(KIND=8) :: YOUT(1:2)
              REAL(KIND=8) :: GN
              REAL(KIND=8) :: HN
            END SUBROUTINE RK4_VALVE
          END INTERFACE 
        END MODULE RK4_VALVE__genmod
