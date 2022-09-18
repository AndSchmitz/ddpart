#Loss efficiency by impaction (E_Im)
#Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK.
#Revisiting particle dry deposition and its role in radiative effect estimates.
#Proceedings of the National Academy of Sciences 2020;117:26076–26082.
#Eq. 4

CalculateLossEfficiencyImpaction <- function(
  StokesNumber,
  ImpactionParameterAlpha
) {
  C_Im <-GetConstants()$C_Im
  beta <- GetConstants()$beta
  E_Im <- C_Im * (StokesNumber / (ImpactionParameterAlpha + StokesNumber))^beta
  return(E_Im)
}
