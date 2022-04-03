#Dynamic viscosity of air
#Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
#2006
#page 909

CalculateDynamicViscosityOfAir <- function(
  T_air_Kelvin
) {
  DynamicViscosityAir <- 1.8 * 1e-5 * (T_air_Kelvin/298)^0.85 #kg/(m*s)
  return(DynamicViscosityAir)
}