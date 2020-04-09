        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 03 20:35:14 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LEAKAGE_M__genmod
          INTERFACE 
            SUBROUTINE LEAKAGE_M(THETA_1,THETA_2,P_SCV,T_SCV,RHO_SCV,   &
     &P_DCV,T_DCV,CV_DCV,CP_DCV,RHO_DCV,H_DCV,S_DCV,MIU_DCV,L_V,        &
     &CL_ROTOR_RAD_DY,DMDTHETA1_LEAK_RAD_RO,DMDTHETA1_LEAK_VEF,         &
     &DMDTHETA1_LEAK_VS,DMDTHETA1_LEAK_REF,DMDTHETA1_LEAK_S,            &
     &DEDTHETA1_LEAK_S,DMDTHETA1_LEAK_D,DEDTHETA1_LEAK_D)
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
              REAL(KIND=8) :: P_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: RHO_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: T_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: CV_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: CP_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: RHO_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: H_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: S_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: MIU_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: L_V(1:NO_DATA+1)
              REAL(KIND=8) :: CL_ROTOR_RAD_DY(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_RAD_RO(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_VEF(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_VS(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_REF(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_S(1:NO_DATA+1)
              REAL(KIND=8) :: DEDTHETA1_LEAK_S(1:NO_DATA+1)
              REAL(KIND=8) :: DMDTHETA1_LEAK_D(1:NO_DATA+1)
              REAL(KIND=8) :: DEDTHETA1_LEAK_D(1:NO_DATA+1)
            END SUBROUTINE LEAKAGE_M
          END INTERFACE 
        END MODULE LEAKAGE_M__genmod
