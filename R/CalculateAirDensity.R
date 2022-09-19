#' @title CalculateAirDensity
#'
#' @description Calculates the density of air according to Seinfeld and Pandis
#' (2006) page 735 eq. 16.36.
#'
#' @param AirPressure_Pa Air pressure in Pa.
#'
#' @param T_air_K Air temperature in K.
#'
#' @return Density of air in kg/m3.
#'
#' @export
#'
#' @references Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change. Wiley; 2006.


CalculateAirDensity <- function(AirPressure_Pa,
                                T_air_K) {
  M <- GetConstants()$M
  R <- GetConstants()$R
  SurfaceAirDensity_kgm3 <- AirPressure_Pa * M / (R * T_air_K)
  return(SurfaceAirDensity_kgm3)
}
