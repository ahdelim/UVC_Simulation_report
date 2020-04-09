        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 23 11:45:12 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE THERMO_WITH_HEAT_LEAKAGE__genmod
          INTERFACE 
            SUBROUTINE THERMO_WITH_HEAT_LEAKAGE(THETA_1,V_COM,V_SUC,    &
     &DQDTHETA_SC,DQDTHETA_DC,DMDTHETA1_SI,DMDTHETA1_SO,DMDTHETA1_DI,   &
     &DMDTHETA1_DO,DMDTHETA1_LEAK_S,DEDTHETA1_LEAK_S,DMDTHETA1_LEAK_D,  &
     &DEDTHETA1_LEAK_D,P_SCV,M_SCV,T_SCV,U_SCV,RHO_SCV,H_SCV,S_SCV,     &
     &MIU_SCV,CV_SCV,CP_SCV,K_SCV,P_DCV,M_DCV,T_DCV,U_DCV,RHO_DCV,H_DCV,&
     &S_DCV,MIU_DCV,CV_DCV,CP_DCV,K_DCV,DMDTHETA1_S,DEDTHETA1_S,        &
     &DMDTHETA1_D,DEDTHETA1_D,DVDTHETA1_S,DVDTHETA1_D,Y_START,Y_MID,    &
     &Y_END)
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
              REAL(KIND=8) :: DQDTHETA_SC(1:NO_DATA+1)
              REAL(KIND=8) :: DQDTHETA_DC(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SO(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DO(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_S(1:NO_DATA+1)
              REAL(KIND=8) :: DEDTHETA1_LEAK_S(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_D(1:NO_DATA+1)
              REAL(KIND=8) :: DEDTHETA1_LEAK_D(1:NO_DATA+1)
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
              REAL(KIND=8) :: DMDTHETA1_S(1:NO_DATA+1)
              REAL(KIND=8) :: DEDTHETA1_S(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_D(1:NO_DATA+1)
              REAL(KIND=8) :: DEDTHETA1_D(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_S(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_D(1:NO_DATA+1)
              REAL(KIND=8) :: Y_START(1:NO_DATA+1)
              REAL(KIND=8) :: Y_MID(1:NO_DATA+1)
              REAL(KIND=8) :: Y_END(1:NO_DATA+1)
            END SUBROUTINE THERMO_WITH_HEAT_LEAKAGE
          END INTERFACE 
        END MODULE THERMO_WITH_HEAT_LEAKAGE__genmod
