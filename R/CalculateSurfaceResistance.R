#' @title CalculateSurfaceResistance
#'
#' @description Calculates the surface resistance (R_s) according to Zhang et
#' al. (2001).
#'
#' @param SurfaceIsWet Indicator whether the receptor surface is wet (for
#' bounce correction term), boolean.
#'
#' @param FrictionVelocity_ms Friction velocity in m/s.
#'
#' @param StokesNumber Stokes number.
#' in m.
#'
#' @param E_b Loss efficiency by brownian diffusion.
#' in m.
#' #'
#' @param E_Im Loss efficiency by impaction.
#' in m.
#' #'
#' @param E_In Loss efficiency by interception.
#' in m.
#'
#' @param Parametrization A character defining which parametrization to use.
#' Valid values are "Emerson20" and "Zhang01"
#'
#' @return Surface resistance in s/m.
#'
#' @export
#'
#' @references Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle
#' dry deposition scheme for an atmospheric aerosol module. Atmospheric
#' Environment 2001;35:549–560.


CalculateSurfaceResistance <- function(SurfaceIsWet,
                                       FrictionVelocity_ms,
                                       StokesNumber,
                                       E_b,
                                       E_Im,
                                       E_In,
                                       Parametrization) {

  # Sanity checks
  InputLength <- length(SurfaceIsWet)
  if (
    (length(FrictionVelocity_ms) != InputLength) |
      (length(StokesNumber) != InputLength) |
      (length(E_b) != InputLength) |
      (length(E_Im) != InputLength) |
      (length(E_In) != InputLength) |
      (length(Parametrization) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  epsilon_0 <- GetParameters(Parametrization, "epsilon_0")


  # Bounce correction
  # Zhang et al. 2001 eq9
  BounceCorrectionTerm <- rep(x = 1, times = InputLength)
  BounceCorrectionTerm[!SurfaceIsWet] <- exp(-sqrt(StokesNumber))

  R_s <- 1 / (epsilon_0 * FrictionVelocity_ms * BounceCorrectionTerm * (E_b + E_Im + E_In))
  return(R_s)
}
