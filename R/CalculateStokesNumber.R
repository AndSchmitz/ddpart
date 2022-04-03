#Stokes number
#Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an
#atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.
CalculateStokesNumber <- function(
  FrictionVelocity_ms,
  SettlingVelocity_ms,
  CharacteristicRadius_m,
  KinematicViscosityOfAir,
  SurfaceIsVegetated
) {
  if ( is.na(SurfaceIsVegetated) ) {
    stop("Could not determine whether surface is vegetated.")
  }
  if ( SurfaceIsVegetated ) {
    StokesNumber <- SettlingVelocity_ms * FrictionVelocity_ms / (GetConstants()$g * CharacteristicRadius_m)
  } else {
    StokesNumber <- SettlingVelocity_ms * FrictionVelocity_ms^2 / KinematicViscosityOfAir
  } 
  return(StokesNumber)
}