        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:59 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DERIVSS__genmod
          INTERFACE 
            SUBROUTINE DERIVSS(DTHETA1,DUDTHE,N,P_CV,DVDTHETA1,DEDTHETA1&
     &,U_CV,DQDTHETA1,DMDTHETA1,M_CV,DRHO,RHO,V_CV)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: DTHETA1
              REAL(KIND=8) :: DUDTHE(1:N)
              REAL(KIND=8) :: P_CV
              REAL(KIND=8) :: DVDTHETA1
              REAL(KIND=8) :: DEDTHETA1
              REAL(KIND=8) :: U_CV
              REAL(KIND=8) :: DQDTHETA1
              REAL(KIND=8) :: DMDTHETA1
              REAL(KIND=8) :: M_CV
              REAL(KIND=8) :: DRHO
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: V_CV
            END SUBROUTINE DERIVSS
          END INTERFACE 
        END MODULE DERIVSS__genmod
