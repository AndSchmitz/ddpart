#' @title CalculateKinematicViscosityOfAir
#'
#' @description Calculates kinematic viscosity of air according to Dixon (2007)
#'   appendix B
#'
#' @param DynamicViscosityAir_kgms Dynamic viscosity of air in kg/(m*s).
#'
#' @param AirDensity_kgm3 Density of air in kg/m3.
#'
#' @return Kinematic viscosity of air in kg/(m*s).
#'
#' @export
#'
#' @references Dixon JC. The Shock Absorber Handbook. John Wiley & Sons; October
#'   22, 2007.

CalculateKinematicViscosityOfAir <- function(DynamicViscosityAir_kgms,
                                             AirDensity_kgm3) {

  # Sanity checks
  InputLength <- length(DynamicViscosityAir_kgms)
  if (
    (length(AirDensity_kgm3) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }


  return(DynamicViscosityAir_kgms / AirDensity_kgm3)
}
