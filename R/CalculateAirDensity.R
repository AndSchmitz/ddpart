#Air density
#Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
#2006
#page 735 below eq. 16.36
CalculateAirDensity <- function(
  AirPressure_Pa,
  T_air_K
) {
  M <- GetConstants()$M
  R <- GetConstants()$R
  SurfaceAirDensity_kgm3 <- AirPressure_Pa * M / (R * T_air_K)
  return(SurfaceAirDensity_kgm3)
}
