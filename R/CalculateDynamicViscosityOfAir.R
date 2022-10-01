#' @title CalculateDynamicViscosityOfAir
#'
#' @description Calculates Dynamic viscosity of air according to Seinfeld and
#'   Pandis (2006) page 909.
#'
#' @param T_air_K Air temperature in Kelvin.
#'
#' @return Dynamic viscosity of air in kg/(m*s).
#'
#' @export
#' @references Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From
#'   Air Pollution to Climate Change. Wiley; 2006.

CalculateDynamicViscosityOfAir <- function(T_air_K) {
  DynamicViscosityAir_kgms <- 1.8 * 1e-5 * (T_air_K / 298)^0.85
  return(DynamicViscosityAir_kgms)
}
