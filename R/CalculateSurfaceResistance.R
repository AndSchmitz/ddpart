#Surface resistance
#Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an
#atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.
CalculateSurfaceResistance <- function(
  SurfaceIsWet,
  FrictionVelocity_ms,
  StokesNumber,
  E_b,
  E_Im,
  E_In,
  Parametrization
) {

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


  #Bounce correction
  #Zhang et al. 2001 eq9
  BounceCorrectionTerm <- rep(x = 1, times = InputLength)
  BounceCorrectionTerm[!SurfaceIsWet] <- exp(-sqrt(StokesNumber))

  R_s <- 1 / (epsilon_0 * FrictionVelocity_ms * BounceCorrectionTerm * (E_b + E_Im + E_In))
  return(R_s)

}
