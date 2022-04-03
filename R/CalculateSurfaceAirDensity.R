#Air density at surface
#Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
#2006
#page 735 below eq. 16.36
CalculateSurfaceAirDensity <- function(
  AirPressure_Pa,
  T_air_K
) {
  SurfaceAirDensity_kgm3 <- AirPressure_Pa * GetConstants()$M / (GetConstants()$R * T_air_K)
  return(SurfaceAirDensity_kgm3)
}
