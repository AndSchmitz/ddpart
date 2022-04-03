#Surface resistance
#Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an
#atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.
CalculateSurfaceResistance <- function(
  SurfaceIsWet,
  FrictionVelocity_ms,
  StokesNumber,
  E_b,
  E_Im,
  E_In
) {
  #Bounce correction
  #Zhang et al. 2001 eq9
  BounceCorrectionTerm <- ifelse(
    test = SurfaceIsWet,
    yes = 1,
    no = exp(-sqrt(StokesNumber))
  )

  epsilon_0 <- GetConstants()$epsilon_0
  R_s <- 1 / (epsilon_0 * FrictionVelocity_ms * BounceCorrectionTerm * (E_b + E_Im + E_In))
  return(R_s)
  
}