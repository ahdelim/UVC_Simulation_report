        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:50 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OPTIMIZATION_POWER_M__genmod
          INTERFACE 
            SUBROUTINE OPTIMIZATION_POWER_M(THETA_1,V_SUC,V_COM,        &
     &P_CSCV_ID,P_CDCV_ID,P_SCV,P_DCV,H_SCV,H_DCV,DMDTHETA1_SI,         &
     &DMDTHETA1_SO,DMDTHETA1_LEAK_S,DMDTHETA1_DI,DMDTHETA1_DO,          &
     &DVDTHETA1_S,DVDTHETA1_D,DLVDT,DGAMMADT_1,F_VHOC_N,F_VHOC_T,F_1_N, &
     &F_2_N,F_CX,F_CY,F_RESULTANT,ECCR,ATT,T_INERTIA_RO,T_INERTIA_VH,   &
     &T_COM_C,L_F_VS,L_EF_VH,L_S_VH,L_EF_RO,L_LUB,P_BEAR_S,L_LIP_SEAL)
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
              REAL(KIND=8) :: V_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: V_COM(1:NO_DATA+1)
              REAL(KIND=8) :: P_CSCV_ID(1:NO_DATA+1)
              REAL(KIND=8) :: P_CDCV_ID(1:NO_DATA+1)
              REAL(KIND=8) :: P_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: H_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: H_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_SO(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_S(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DI(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_DO(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_S(1:NO_DATA+1)
              REAL(KIND=8) :: DVDTHETA1_D(1:NO_DATA+1)
              REAL(KIND=8) :: DLVDT(1:NO_DATA+1)
              REAL(KIND=8) :: DGAMMADT_1(1:NO_DATA+1)
              REAL(KIND=8) :: F_VHOC_N(1:NO_DATA+1)
              REAL(KIND=8) :: F_VHOC_T(1:NO_DATA+1)
              REAL(KIND=8) :: F_1_N(1:NO_DATA+1)
              REAL(KIND=8) :: F_2_N(1:NO_DATA+1)
              REAL(KIND=8) :: F_CX(1:NO_DATA+1)
              REAL(KIND=8) :: F_CY(1:NO_DATA+1)
              REAL(KIND=8) :: F_RESULTANT(1:NO_DATA+1)
              REAL(KIND=8) :: ECCR(1:NO_DATA+1)
              REAL(KIND=8) :: ATT(1:NO_DATA+1)
              REAL(KIND=8) :: T_INERTIA_RO(1:NO_DATA+1)
              REAL(KIND=8) :: T_INERTIA_VH(1:NO_DATA+1)
              REAL(KIND=8) :: T_COM_C(1:NO_DATA+1)
              REAL(KIND=8) :: L_F_VS(1:NO_DATA+1)
              REAL(KIND=8) :: L_EF_VH(1:NO_DATA+1)
              REAL(KIND=8) :: L_S_VH(1:NO_DATA+1)
              REAL(KIND=8) :: L_EF_RO(1:NO_DATA+1)
              REAL(KIND=8) :: L_LUB(1:NO_DATA+1)
              REAL(KIND=8) :: P_BEAR_S(1:NO_DATA+1)
              REAL(KIND=8) :: L_LIP_SEAL(1:NO_DATA+1)
            END SUBROUTINE OPTIMIZATION_POWER_M
          END INTERFACE 
        END MODULE OPTIMIZATION_POWER_M__genmod
