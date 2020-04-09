        !COMPILER-GENERATED INTERFACE MODULE: Thu Nov 28 15:02:36 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DYNAMIC_M2__genmod
          INTERFACE 
            SUBROUTINE DYNAMIC_M2(THETA_1,THETA_2,GAMMA_1,L_V,P_SCV,    &
     &P_DCV,DGAMMADTT_1,F_VHOC_N,F_VHOC_T,F_1_N,F_2_N,F_CX,F_CY,        &
     &F_RESULTANT,T_INERTIA_RO,T_INERTIA_VH,T_COM_C)
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
              REAL(KIND=8) :: GAMMA_1(1:NO_DATA+1)
              REAL(KIND=8) :: L_V(1:NO_DATA+1)
              REAL(KIND=8) :: P_SCV(1:NO_DATA+1)
              REAL(KIND=8) :: P_DCV(1:NO_DATA+1)
              REAL(KIND=8) :: DGAMMADTT_1(1:NO_DATA+1)
              REAL(KIND=8) :: F_VHOC_N(1:NO_DATA+1)
              REAL(KIND=8) :: F_VHOC_T(1:NO_DATA+1)
              REAL(KIND=8) :: F_1_N(1:NO_DATA+1)
              REAL(KIND=8) :: F_2_N(1:NO_DATA+1)
              REAL(KIND=8) :: F_CX(1:NO_DATA+1)
              REAL(KIND=8) :: F_CY(1:NO_DATA+1)
              REAL(KIND=8) :: F_RESULTANT(1:NO_DATA+1)
              REAL(KIND=8) :: T_INERTIA_RO(1:NO_DATA+1)
              REAL(KIND=8) :: T_INERTIA_VH(1:NO_DATA+1)
              REAL(KIND=8) :: T_COM_C(1:NO_DATA+1)
            END SUBROUTINE DYNAMIC_M2
          END INTERFACE 
        END MODULE DYNAMIC_M2__genmod
