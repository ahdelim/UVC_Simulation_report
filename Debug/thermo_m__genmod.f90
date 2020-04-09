        !COMPILER-GENERATED INTERFACE MODULE: Tue Feb 04 01:32:49 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE THERMO_M__genmod
          INTERFACE 
            SUBROUTINE THERMO_M(THETA_1,THETA_2,DLVDT,DGAMMADT_1,V_COM, &
     &V_SUC,L_V,P_CSCV_ID,P_CDCV_ID,DVDTHETA1_S,DVDTHETA1_D,DQDTHETA_SC,&
     &DQDTHETA_DC,DMDTHETA1_SI,DMDTHETA1_SO,DMDTHETA1_LEAK_S,           &
     &DMDTHETA1_DI,DMDTHETA1_DO,P_SCV,M_SCV,T_SCV,U_SCV,RHO_SCV,H_SCV,  &
     &S_SCV,MIU_SCV,CV_SCV,CP_SCV,K_SCV,P_DCV,M_DCV,T_DCV,U_DCV,RHO_DCV,&
     &H_DCV,S_DCV,MIU_DCV,CV_DCV,CP_DCV,K_DCV,Q_DCSC_EX,Q_SCHC_EX,      &
     &Q_SCRO_EX,Q_SCHC_VD_EX,Q_SCOIL_VD_EX,Q_DCHC_EX,Q_DCRO_EX,         &
     &Q_DCHC_VD_EX,Q_DCOIL_VD_EX)
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
              REAL(KIND=8) :: DLVDT(1:NO_DATA+1)
              REAL(KIND=8) :: DGAMMADT_1(1:NO_DATA+1)
              REAL(KIND=8) :: V_COM(1:NO_DATA+1)
              REAL(KIND=8) :: V_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: L_V(1:NO_DATA+1)
              REAL(KIND=8) :: P_CSCV_ID(1:NO_DATA+1)
              REAL(KIND=8) :: P_CDCV_ID(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_S(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_D(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_SC(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_DC(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SO(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_S(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DO(1:NO_DATA+1)
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
              REAL(KIND=8) :: Q_DCSC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCHC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCRO_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCHC_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCOIL_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCHC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCRO_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCHC_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCOIL_VD_EX(1:NO_DATA+1)
            END SUBROUTINE THERMO_M
          END INTERFACE 
        END MODULE THERMO_M__genmod
