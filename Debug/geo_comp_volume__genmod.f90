        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:58 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GEO_COMP_VOLUME__genmod
          INTERFACE 
            SUBROUTINE GEO_COMP_VOLUME(V_SUC,V_COM,THETA_1,THETA_2,L_V, &
     &GAMMA_1,L_COM,R_OC,R_RO,R_VH,L_VH,E,W_V_RO,W_VS_RO,L_VS_RO,       &
     &W_VSRO_GAP,L_VSRO_GAP,DIA_SUC,DIA_DISC,LENGTH_SUC,LENGTH_DISC,    &
     &W_VS_OIL_PATH,H_VS_OIL_PATH,L_VS_OIL_PATH)
              COMMON/SIMULATION_PARA_2/ NO_DATA,NO_DATA_LOW,NO_DATA_HIGH&
     &,DATA_STEP,DATA_STEP_SUCT,DATA_STEP_HIGH,MAX_WRITE_DATA
                INTEGER(KIND=4) :: NO_DATA
                INTEGER(KIND=4) :: NO_DATA_LOW
                INTEGER(KIND=4) :: NO_DATA_HIGH
                REAL(KIND=4) :: DATA_STEP
                REAL(KIND=4) :: DATA_STEP_SUCT
                REAL(KIND=4) :: DATA_STEP_HIGH
                REAL(KIND=4) :: MAX_WRITE_DATA
              REAL(KIND=8) :: V_SUC(1:NO_DATA+1)
              REAL(KIND=8) :: V_COM(1:NO_DATA+1)
              REAL(KIND=8) :: THETA_1(1:NO_DATA+1)
              REAL(KIND=8) :: THETA_2(1:NO_DATA+1)
              REAL(KIND=8) :: L_V(1:NO_DATA+1)
              REAL(KIND=8) :: GAMMA_1(1:NO_DATA+1)
              REAL(KIND=8) :: L_COM
              REAL(KIND=8) :: R_OC
              REAL(KIND=8) :: R_RO
              REAL(KIND=8) :: R_VH
              REAL(KIND=8) :: L_VH
              REAL(KIND=8) :: E
              REAL(KIND=8) :: W_V_RO
              REAL(KIND=8) :: W_VS_RO
              REAL(KIND=8) :: L_VS_RO
              REAL(KIND=8) :: W_VSRO_GAP
              REAL(KIND=8) :: L_VSRO_GAP
              REAL(KIND=8) :: DIA_SUC
              REAL(KIND=8) :: DIA_DISC
              REAL(KIND=8) :: LENGTH_SUC
              REAL(KIND=8) :: LENGTH_DISC
              REAL(KIND=8) :: W_VS_OIL_PATH
              REAL(KIND=8) :: H_VS_OIL_PATH
              REAL(KIND=8) :: L_VS_OIL_PATH
            END SUBROUTINE GEO_COMP_VOLUME
          END INTERFACE 
        END MODULE GEO_COMP_VOLUME__genmod
