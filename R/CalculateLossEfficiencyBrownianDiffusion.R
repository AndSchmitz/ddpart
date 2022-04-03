#Loss efficiency by brownian diffusion (E_b)
#Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK.
#Revisiting particle dry deposition and its role in radiative effect estimates.
#Proceedings of the National Academy of Sciences 2020;117:26076–26082.
#Eq. 3

CalculateLossEfficiencyBrownianDiffusion <- function(
  SchmidtNumber
) {
  C_b <- GetConstants()$C_b_E20 
  Gamma <- GetConstants()$Gamma_E20
  E_b = C_b * SchmidtNumber^(-Gamma)
  return(E_b)
}