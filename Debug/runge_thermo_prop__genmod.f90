        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:59 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RUNGE_THERMO_PROP__genmod
          INTERFACE 
            SUBROUTINE RUNGE_THERMO_PROP(DQDTHETA1,DMDTHETA1,DEDTHETA1, &
     &P_CV,DVDTHETA1,M_CV,U_CV,UN,V_CV,RHON)
              REAL(KIND=8) :: DQDTHETA1
              REAL(KIND=8) :: DMDTHETA1
              REAL(KIND=8) :: DEDTHETA1
              REAL(KIND=8) :: P_CV
              REAL(KIND=8) :: DVDTHETA1
              REAL(KIND=8) :: M_CV
              REAL(KIND=8) :: U_CV
              REAL(KIND=8) :: UN
              REAL(KIND=8) :: V_CV
              REAL(KIND=8) :: RHON
            END SUBROUTINE RUNGE_THERMO_PROP
          END INTERFACE 
        END MODULE RUNGE_THERMO_PROP__genmod
