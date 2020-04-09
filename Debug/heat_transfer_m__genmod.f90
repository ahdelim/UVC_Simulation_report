        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:52 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HEAT_TRANSFER_M__genmod
          INTERFACE 
            SUBROUTINE HEAT_TRANSFER_M(THETA_1,THETA_2,V_COM,V_SUC,DLVDT&
     &,DGAMMADT_1,L_V,P_SCV,M_SCV,T_SCV,U_SCV,RHO_SCV,H_SCV,S_SCV,      &
     &MIU_SCV,CV_SCV,CP_SCV,K_SCV,P_DCV,M_DCV,T_DCV,U_DCV,RHO_DCV,H_DCV,&
     &S_DCV,MIU_DCV,CV_DCV,CP_DCV,K_DCV,DQDTHETA_SC,DQDTHETA_DC,        &
     &DQDTHETA_HC,DQDTHETA_RESOIL,DQDTHETA_OIL,T_OCSC,T_OCDC,T_ROSC,    &
     &T_RODC,T_ROLLER,T_UOCSC,T_LOCSC,T_UOCDC,T_LOCDC,T_OCSC2,T_OCSC3,  &
     &T_OCDC2,T_OCDC3,T_ROSC2,T_ROSC3,T_RODC2,T_RODC3,T_ROLLER2,        &
     &T_ROLLER3,Q_DCSC_EX,Q_SCHC_EX,Q_SCRO_EX,Q_SCHC_VD_EX,Q_SCOIL_VD_EX&
     &,Q_DCHC_EX,Q_DCRO_EX,Q_DCHC_VD_EX,Q_DCOIL_VD_EX)
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
              REAL(KIND=8) :: THETA_1(1:NO_DATA+1)
              REAL(KIND=8) :: THETA_2(1:NO_DATA+1)
              REAL(KIND=8) :: V_COM(1:NO_DATA+1)
              REAL(KIND=8) :: V_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: DLVDT(1:NO_DATA+1)
              REAL(KIND=8) :: DGAMMADT_1(1:NO_DATA+1)
              REAL(KIND=8) :: L_V(1:NO_DATA+1)
              REAL(KIND=8) :: P_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: M_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: U_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: RHO_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: H_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: S_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: MIU_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: CV_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: CP_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: K_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: M_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: U_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: RHO_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: H_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: S_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: MIU_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: CV_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: CP_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: K_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_SC(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_DC(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_HC(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_RESOIL(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_OIL(1:NO_DATA+1)
              REAL(KIND=8) :: T_OCSC(1:NO_DATA+1)
              REAL(KIND=8) :: T_OCDC(1:NO_DATA+1)
              REAL(KIND=8) :: T_ROSC(1:NO_DATA+1)
              REAL(KIND=8) :: T_RODC(1:NO_DATA+1)
              REAL(KIND=8) :: T_ROLLER(1:NO_DATA+1)
              REAL(KIND=8) :: T_UOCSC(1:NO_DATA+1)
              REAL(KIND=8) :: T_LOCSC(1:NO_DATA+1)
              REAL(KIND=8) :: T_UOCDC(1:NO_DATA+1)
              REAL(KIND=8) :: T_LOCDC(1:NO_DATA+1)
              REAL(KIND=8) :: T_OCSC2(1:NO_DATA+1)
              REAL(KIND=8) :: T_OCSC3(1:NO_DATA+1)
              REAL(KIND=8) :: T_OCDC2(1:NO_DATA+1)
              REAL(KIND=8) :: T_OCDC3(1:NO_DATA+1)
              REAL(KIND=8) :: T_ROSC2(1:NO_DATA+1)
              REAL(KIND=8) :: T_ROSC3(1:NO_DATA+1)
              REAL(KIND=8) :: T_RODC2(1:NO_DATA+1)
              REAL(KIND=8) :: T_RODC3(1:NO_DATA+1)
              REAL(KIND=8) :: T_ROLLER2(1:NO_DATA+1)
              REAL(KIND=8) :: T_ROLLER3(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCSC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCHC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCRO_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCHC_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCOIL_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCHC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCRO_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCHC_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCOIL_VD_EX(1:NO_DATA+1)
            END SUBROUTINE HEAT_TRANSFER_M
          END INTERFACE 
        END MODULE HEAT_TRANSFER_M__genmod
