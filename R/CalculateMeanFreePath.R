#' @title CalculateMeanFreePath
#'
#' @description Calculates the mean free path of an air molecule according to
#' Seinfeld and Pandis (2006) page 399 eq. 9.6.
#'
#' @param T_air_K Air temperature in Kelvin.
#'
#' @param AirPressure_Pa Air pressure in Pa.
#'
#' @param DynamicViscosityAir_kgms Dynamic viscosity of air in kg/(m*s). E.g. provided by CalculateDynamicViscosityOfAir().
#'
#' @return Mean free path of an air molecule in m.
#'
#' @examples
#' # Validation against example Seinfeld and Pandis eq 9.7 page 399
#' T_air_K <- 298
#' AirPressure_Pa <- 101325
#' DynamicViscosityAir_kgms <- 1.8e-5
#' MeanFreePath_m <- CalculateMeanFreePath(T_air_K, AirPressure_Pa, DynamicViscosityAir_kgms)
#' MeanFreePath_um <- MeanFreePath_m * 1e6
#' print(MeanFreePath_um)
#' # 0.0651 um as given in SP06
#'
#' @export
#'
#' @references Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.

CalculateMeanFreePath <- function(T_air_K,
                                  AirPressure_Pa,
                                  DynamicViscosityAir_kgms) {

  # Sanity checks
  InputLength <- length(T_air_K)
  if (
    (length(AirPressure_Pa) != InputLength) |
      (length(DynamicViscosityAir_kgms) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  R <- GetConstants()$R
  M_b <- GetConstants()$M
  # Seinfeld and Pandis eq. 9.6
  MeanFreePath_m <- 2 * DynamicViscosityAir_kgms / (AirPressure_Pa * sqrt(8 * M_b / (pi * R * T_air_K)))
  return(MeanFreePath_m)
}
