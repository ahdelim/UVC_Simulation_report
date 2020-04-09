        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE THERM__genmod
          INTERFACE 
            SUBROUTINE THERM(T,RHO,X,P,E,H,S,CV,CP,W,HJT)
              REAL(KIND=8) :: T
              REAL(KIND=8) :: RHO
              REAL(KIND=8) :: X(20)
              REAL(KIND=8) :: P
              REAL(KIND=8) :: E
              REAL(KIND=8) :: H
              REAL(KIND=8) :: S
              REAL(KIND=8) :: CV
              REAL(KIND=8) :: CP
              REAL(KIND=8) :: W
              REAL(KIND=8) :: HJT
            END SUBROUTINE THERM
          END INTERFACE 
        END MODULE THERM__genmod
