        !COMPILER-GENERATED INTERFACE MODULE: Thu Jun 20 21:46:11 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OPTIMIZATION_CONV_CHECK__genmod
          INTERFACE 
            SUBROUTINE OPTIMIZATION_CONV_CHECK(OPT_CONV_ERROR,          &
     &ARRAY_OBJ_FUNC,ARRAY_SIZE,BEST,BEST_INDEX,WORST,WORST_INDEX)
              INTEGER(KIND=4) :: ARRAY_SIZE
              REAL(KIND=8) :: OPT_CONV_ERROR
              REAL(KIND=8) :: ARRAY_OBJ_FUNC(1:ARRAY_SIZE)
              REAL(KIND=8) :: BEST
              INTEGER(KIND=4) :: BEST_INDEX(1:1)
              REAL(KIND=8) :: WORST
              INTEGER(KIND=4) :: WORST_INDEX(1:1)
            END SUBROUTINE OPTIMIZATION_CONV_CHECK
          END INTERFACE 
        END MODULE OPTIMIZATION_CONV_CHECK__genmod
