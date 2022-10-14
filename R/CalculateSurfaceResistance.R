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
#'
#' @param E_b Loss efficiency by brownian diffusion.
#'
#' @param E_Im Loss efficiency by impaction.
#'
#' @param E_In Loss efficiency by interception.
#'
#' @param ParticleDiameter_m Particle diameter in m.
#'
#' @param Parametrization A character defining which parametrization to use.
#' See ?GetLandUseParameters for a list of valid values.
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
                                       ParticleDiameter_m,
                                       Parametrization) {

  # Sanity checks
  InputLength <- length(SurfaceIsWet)
  if (
    (length(FrictionVelocity_ms) != InputLength) |
      (length(StokesNumber) != InputLength) |
      (length(E_b) != InputLength) |
      (length(E_Im) != InputLength) |
      (length(E_In) != InputLength) |
      (length(ParticleDiameter_m) != InputLength) |
      (length(Parametrization) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  #Get parameter epsilon
  epsilon_0 <- GetParameters(Parametrization, "epsilon_0")

  #Bounce correction
  #Zhang et al. 2001 eq9
  #"Particles larger than 5 um may rebound after hitting a surface."
  #"[...] thus the same formula is used in the present study with the condition
  #that no particles rebound from a wet surface."
  #Set bounce correction to 1 (no re-bounce)
  BounceCorrectionTerm <- rep(x = 1, times = InputLength)
  #Modify value at rows where surface is dry
  BounceCorrectionTerm[!SurfaceIsWet] <- exp(-sqrt(StokesNumber[!SurfaceIsWet]))
  #Disable bounce correction again at rows where particles are smaller than
  #5 um diameter
  idx_small <- which(ParticleDiameter_m <= 5e-6)
  BounceCorrectionTerm[idx_small] <- 1

  #Calculate surface resistance according to Zhang et al. (2001) eq. 5
  R_s <- 1 / (epsilon_0 * FrictionVelocity_ms * BounceCorrectionTerm * (E_b + E_Im + E_In))
  return(R_s)
}
