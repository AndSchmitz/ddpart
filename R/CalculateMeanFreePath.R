#Mean free path of an air molecule
#Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
#2006
#page 399
CalculateMeanFreePath <- function(
  T_air_K,
  AirPressure_Pa,
  DynamicViscosityAir
) {
  R <- GetConstants()$R
  M_b <- GetConstants()$M
  #Seinfeld and Pandis eq. 9.6
  lambda_air <- 2 * DynamicViscosityAir / ( AirPressure_Pa * sqrt( 8 * M_b  / (pi * R * T_air_K) ) )
  return(lambda_air)
}

#Validation against example SP06 eq 9.7 page 399
# T_air_K <- 298
# AirPressure_Pa = 101325
# DynamicViscosityAir = 1.8e-5
# CalculateMeanFreePath(T_air_K,AirPressure_Pa,DynamicViscosityAir) * 1e6
#0.0651 um as given in SP06
