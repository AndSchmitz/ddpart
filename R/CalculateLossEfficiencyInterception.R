#' @title CalculateLossEfficiencyInterception
#'
#' @description Calculates the loss efficiency by interception (E_In) according
#' to Emerson et al. (2020)
#'
#' @param ParticleDiameter_m Particle diameter in m.
#'
#' @param CharacteristicRadius_m Characteristic radius of the receptor surfaces
#' in m.
#'
#' @param Parametrization A character defining which parametrization to use.
#' Valid values are "Emerson20" and "Zhang01"
#'
#' @return Loss efficiency by interception.
#'
#' @export
#' @references Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR,
#'  McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in
#'  radiative effect estimates. Proceedings of the National Academy of Sciences
#'  2020;117:26076–26082.


CalculateLossEfficiencyInterception <- function(ParticleDiameter_m,
                                                CharacteristicRadius_m,
                                                Parametrization) {

  # Sanity checks
  InputLength <- length(ParticleDiameter_m)
  if (
    (length(CharacteristicRadius_m) != InputLength) |
      (length(Parametrization) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  # Loss efficiency from Interception
  nu <- GetParameters(Parametrization, "nu")
  C_In <- GetParameters(Parametrization, "C_In")
  E_In <- C_In * (ParticleDiameter_m / CharacteristicRadius_m)^nu
  return(E_In)
}
