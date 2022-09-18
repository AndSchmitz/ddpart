#Loss efficiency by brownian diffusion (E_b)
#Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK.
#Revisiting particle dry deposition and its role in radiative effect estimates.
#Proceedings of the National Academy of Sciences 2020;117:26076–26082.
#Eq. 3

CalculateLossEfficiencyBrownianDiffusion <- function(
  SchmidtNumber,
  BrownianDiffusionParameterGamma
) {
  C_b <- GetConstants()$C_b
  E_b = C_b * SchmidtNumber^(-BrownianDiffusionParameterGamma)
  return(E_b)
}
