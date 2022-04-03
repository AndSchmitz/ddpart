#Gravitational settling velocity
#Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an
#atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.

CalculateSettlingVelocity <- function(
  ParticleDensity_kgm3, #density of particle
  ParticleDiameter_m, #particle diameter
  T_air_K,
  AirPressure_Pa,
  MeanFreePathOfAirMolecule_m,
  DynamicViscosityAir
) {

  #Correction factor for small particles
  #Zhang et al. 2001 eq. 3
  C <- 1 + 2 * MeanFreePathOfAirMolecule_m / ParticleDiameter_m * (1.257 + 0.4 * exp(-0.55 * ParticleDiameter_m / MeanFreePathOfAirMolecule_m))

  #Gravitational settling velocity
  #Zhang et al. 2001 eq. 2
  V_g <- ParticleDensity_kgm3 * ParticleDiameter_m^2 * GetConstants()$g * C / (18 * DynamicViscosityAir)

  return(V_g)
}
