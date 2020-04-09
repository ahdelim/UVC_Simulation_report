! include "var_indicated_work.f90"
    double precision P_ind, P_ind_id, P_loss_suc, P_loss_disc       ! for no heat and leakage case
    double precision P_ind_hl, P_loss_suc_hl, P_loss_disc_hl
    double precision P_comp_loss, P_comp_loss_hl
    common/indicated_work_done/P_ind, P_ind_id, P_loss_suc, P_loss_disc
    common/indicated_work_done_2/P_ind_hl, P_loss_suc_hl, P_loss_disc_hl
    common/indicated_work_done_3/P_comp_loss, P_comp_loss_hl