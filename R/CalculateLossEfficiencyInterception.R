#Loss efficiency by interception (E_In)
#Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK.
#Revisiting particle dry deposition and its role in radiative effect estimates.
#Proceedings of the National Academy of Sciences 2020;117:26076–26082.
#Eq. 5

CalculateLossEfficiencyInterception <- function(
  ParticleDiameter_m,
  CharacteristicRadius_m,
  Parametrization
) {

  #Sanity checks
  InputLength = length(ParticleDiameter_m)
  if (
    (length(CharacteristicRadius_m) != InputLength) |
    (length(Parametrization) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  #Loss efficiency from Interception
  nu <- GetParameters(Parametrization, "nu")
  C_In <- GetParameters(Parametrization, "C_In")
  E_In <- C_In * (ParticleDiameter_m / CharacteristicRadius_m)^nu
  return(E_In)
}
