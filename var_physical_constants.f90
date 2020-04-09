double precision pi, g_grav, R_universal, R_specific, rho_mat, k_mat                 ! g_grav = gravitational acceleration, rho_mat = density of material, k_mat = thermal conductivity
double precision eff_is                 ! Isentropic efficiency
!double precision coef_vs, coef_vhs, coef_circ_lip                ! Vane side frictional coefficient (coef_vs), Vane tip (coef_vtip)

! ----------------------------------
! 4.6e-05 [m2/s] density: 861[kg/m3], PAG Oil 46, (40 Celsius) compatible with R1234yf/R134a, http://www.behrhellaservice.com/behr-hella-service/assets/media/1065_KlimaKompressoroel_BHS_EN.pdf
! 4.4e-05 [m2/s] density: 993[kg/m3], RENISO PAG 1234, http://fuchs.pt/upload/BR_RENISO_Refrigerant_oils_2014_5.pdf 
!   https://www.engineeringtoolbox.com/cubical-expansion-coefficients-d_1262.html
! https://books.google.com.sg/books?id=J_AkNu-Y1wQC&pg=PA261&lpg=PA261&dq=polyol+ester+heat+capacity&source=bl&ots=j2zw9OEUew&sig=FodF7gb25LpwNsCSNwDobUUN-nY&hl=en&sa=X&ved=0ahUKEwjNq5Cth4TZAhUHbo8KHbVRDkcQ6AEILzAB#v=onepage&q=polyol%20ester%20heat%20capacity&f=false
! ----------------------------------


double precision miu_oil, rho_oil, beta_oil, cp_oil, k_oil       ! lubricant/oil properties   

common/physical_constant/pi, g_grav, R_universal, R_specific
common/mat_properties/rho_mat, k_mat
common/mcrc_eff/eff_is
!common/fric_coef/coef_vs, coef_vhs, coef_circ_lip
common/lubricant_prop/miu_oil, rho_oil, beta_oil, cp_oil, k_oil

! include "var_physical_constants.f90"