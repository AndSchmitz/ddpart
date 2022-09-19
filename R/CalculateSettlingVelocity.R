#' @title CalculateSettlingVelocity
#'
#' @description Calculates settling velocity of particles by gravitation according to Zhang et al. (2001).
#'
#' @param ParticleDensity_kgm3 Particle density in kg/m3.
#'
#' @param ParticleDiameter_m Particle diameter in m.
#'
#' @param MeanFreePathOfAirMolecule_m Mean free path of an air molecule in m. E.g. provided by CalculateMeanFreePath().
#'
#' @param DynamicViscosityAir_kgms Dynamic viscosity of air in kg/(m*s). E.g. provided by CalculateDynamicViscosityOfAir().
#'
#' @return Gravitation settling velocity in m/s.
#'
#' @export
#'
#' @references Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.



CalculateSettlingVelocity <- function(ParticleDensity_kgm3,
                                      ParticleDiameter_m,
                                      MeanFreePathOfAirMolecule_m,
                                      DynamicViscosityAir_kgms) {

  # Sanity checks
  InputLength <- length(ParticleDensity_kgm3)
  if (
    (length(ParticleDiameter_m) != InputLength) |
      (length(MeanFreePathOfAirMolecule_m) != InputLength) |
      (length(DynamicViscosityAir_kgms) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }


  # Correction factor for small particles
  # Zhang et al. 2001 eq. 3
  C <- 1 + 2 * MeanFreePathOfAirMolecule_m / ParticleDiameter_m * (1.257 + 0.4 * exp(-0.55 * ParticleDiameter_m / MeanFreePathOfAirMolecule_m))

  # Gravitational settling velocity
  # Zhang et al. 2001 eq. 2
  g <- GetConstants()$g
  V_g <- ParticleDensity_kgm3 * ParticleDiameter_m^2 * g * C / (18 * DynamicViscosityAir_kgms)

  return(V_g)
}
