#Kinematic viscosity of air
#Dixon JC. The Shock Absorber Handbook. John Wiley & Sons; October 22, 2007.
#Appendix B
CalculateKinematicViscosityOfAir <- function(
  DynamicViscosityAir,
  AirDensity_kgm3
) {
  return(DynamicViscosityAir/AirDensity_kgm3)
}