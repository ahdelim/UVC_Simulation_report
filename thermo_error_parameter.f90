subroutine thermo_error_para(k)
    
    include "var_thermo_error_criterion.f90"
    !double precision error_pos, error_poc, error_tos, error_toc
    !double precision error_pis, error_pic, error_tis, error_tic
    k = 1

    ! Set criterion
    !error_default = 1.0
    !conv_criterion = 0.01          ! convergence criterion in (1.0 %)
    
    read(114,*) error_default       ! default error
    read(114,*) conv_criterion      ! convergence criterion (1.0%)
    close(114)
    ! Error evaluation initialization
    !------- Compressor error check assigning ------- !
    error_pos = error_default       ! *100 to be percentage (100.0% as initial) 
    error_poc = error_default
    error_tos = error_default
    error_toc = error_default

    
endsubroutine
    
    