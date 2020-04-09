        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:52 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE EXERGY_MAIN__genmod
          INTERFACE 
            SUBROUTINE EXERGY_MAIN(THETA_1,V_COM,V_SUC,P_SCV,P_DCV,T_SCV&
     &,T_DCV,S_SCV,S_DCV,H_SCV,H_DCV,U_SCV,U_DCV,M_SCV,M_DCV,           &
     &DMDTHETA1_SI,DMDTHETA1_SO,DMDTHETA1_LEAK_S,DMDTHETA1_DI,          &
     &DMDTHETA1_DO,DQDTHETA_SC,DQDTHETA_DC,Q_DCSC_EX,Q_SCHC_EX,Q_SCRO_EX&
     &,Q_SCHC_VD_EX,Q_SCOIL_VD_EX,Q_DCHC_EX,Q_DCRO_EX,Q_DCHC_VD_EX,     &
     &Q_DCOIL_VD_EX,L_F_VS,L_EF_VH,L_S_VH,L_EF_RO,L_LUB,P_BEAR_S,       &
     &L_LIP_SEAL)
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
              REAL(KIND=8) :: V_COM(1:NO_DATA+1)
              REAL(KIND=8) :: V_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: P_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: S_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: S_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: H_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: H_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: U_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: U_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: M_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: M_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SO(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_S(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DO(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_SC(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_DC(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCSC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCHC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCRO_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCHC_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_SCOIL_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCHC_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCRO_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCHC_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: Q_DCOIL_VD_EX(1:NO_DATA+1)
              REAL(KIND=8) :: L_F_VS(1:NO_DATA+1)
              REAL(KIND=8) :: L_EF_VH(1:NO_DATA+1)
              REAL(KIND=8) :: L_S_VH(1:NO_DATA+1)
              REAL(KIND=8) :: L_EF_RO(1:NO_DATA+1)
              REAL(KIND=8) :: L_LUB(1:NO_DATA+1)
              REAL(KIND=8) :: P_BEAR_S(1:NO_DATA+1)
              REAL(KIND=8) :: L_LIP_SEAL(1:NO_DATA+1)
            END SUBROUTINE EXERGY_MAIN
          END INTERFACE 
        END MODULE EXERGY_MAIN__genmod
