        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DISCHARGE_VALVE_DEFLECTION__genmod
          INTERFACE 
            SUBROUTINE DISCHARGE_VALVE_DEFLECTION(NO_DATA,F_START,F_MID,&
     &F_END,INTEGRAL_F,Y_STOP,D_PORT,A_CROSS_DV,RHO_DV,OMEGA_RESPONSE_DV&
     &,DPRATIO_DV,P_DCV,P_DISC,G_R,H_R,YSTARTN,YENDN,GN,HN)
              INTEGER(KIND=4) :: NO_DATA
              REAL(KIND=8) :: F_START
              REAL(KIND=8) :: F_MID
              REAL(KIND=8) :: F_END
              REAL(KIND=8) :: INTEGRAL_F
              REAL(KIND=8) :: Y_STOP
              REAL(KIND=8) :: D_PORT
              REAL(KIND=8) :: A_CROSS_DV
              REAL(KIND=8) :: RHO_DV
              REAL(KIND=8) :: OMEGA_RESPONSE_DV
              REAL(KIND=8) :: DPRATIO_DV
              REAL(KIND=8) :: P_DCV
              REAL(KIND=8) :: P_DISC
              REAL(KIND=8) :: G_R
              REAL(KIND=8) :: H_R
              REAL(KIND=8) :: YSTARTN
              REAL(KIND=8) :: YENDN
              REAL(KIND=8) :: GN
              REAL(KIND=8) :: HN
            END SUBROUTINE DISCHARGE_VALVE_DEFLECTION
          END INTERFACE 
        END MODULE DISCHARGE_VALVE_DEFLECTION__genmod
