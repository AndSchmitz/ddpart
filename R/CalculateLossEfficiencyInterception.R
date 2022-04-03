#Loss efficiency by interception (E_In)
#Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK.
#Revisiting particle dry deposition and its role in radiative effect estimates.
#Proceedings of the National Academy of Sciences 2020;117:26076–26082.
#Eq. 5

CalculateLossEfficiencyInterception <- function(
  ParticleDiameter_m,
  CharacteristicRadius_m
) {
  #Loss efficiency from Interception
  nu <- GetConstants()$nu_E20
  C_In <- GetConstants()$C_In_E20
  E_In <- C_In * (ParticleDiameter_m / CharacteristicRadius_m)^nu
  return(E_In)
}
