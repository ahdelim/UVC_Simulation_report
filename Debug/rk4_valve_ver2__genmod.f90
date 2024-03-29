        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:47:00 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RK4_VALVE_VER2__genmod
          INTERFACE 
            SUBROUTINE RK4_VALVE_VER2(G,H,G_R,H_R,DGDT,N,DHDT,P_DCV,    &
     &P_DISC,RHO_DV,DPRATIO_DV,OMEGA_NATURAL_DV,F_UP_DV,F_DOWN_DV,      &
     &F_MODE_SHAPE_SQ2,FC_MASS_DV,F_VIS,F_TEN,Y_MID,YOUT,GN,HN)
              REAL(KIND=8) :: G
              REAL(KIND=8) :: H
              REAL(KIND=8) :: G_R
              REAL(KIND=8) :: H_R
              REAL(KIND=8) :: DGDT
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: DHDT
              REAL(KIND=8) :: P_DCV
              REAL(KIND=8) :: P_DISC
              REAL(KIND=8) :: RHO_DV
              REAL(KIND=8) :: DPRATIO_DV
              REAL(KIND=8) :: OMEGA_NATURAL_DV
              REAL(KIND=8) :: F_UP_DV
              REAL(KIND=8) :: F_DOWN_DV
              REAL(KIND=8) :: F_MODE_SHAPE_SQ2
              REAL(KIND=8) :: FC_MASS_DV
              REAL(KIND=8) :: F_VIS
              REAL(KIND=8) :: F_TEN
              REAL(KIND=8) :: Y_MID
              REAL(KIND=8) :: YOUT(1:2)
              REAL(KIND=8) :: GN
              REAL(KIND=8) :: HN
            END SUBROUTINE RK4_VALVE_VER2
          END INTERFACE 
        END MODULE RK4_VALVE_VER2__genmod
