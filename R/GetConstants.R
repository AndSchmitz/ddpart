#' @title GetConstants
#'
#' @description This function returns some constants.
#'
#' @return A named list of constants.
#'
#' @examples GetConstants()
#' @export


GetConstants <- function() {
  Constants <- list(
    # Precision used for rounding at various occasions in this package
    RoundingPrecision = 10,
    g = 9.81,
    # Von Karman constant
    # Seinfeld and Pandis (2006) page 744
    kappa = 0.40,
    # Universal gas constant
    R = 8.314, # m3 * Pa / (K * mol)
    # Molar mass of dry air
    # Seinfeld and Pandis eq. 1.2
    M = 28.97 / 1000, # kg/mol
    # Boltzmann's constant
    k = 1.38 * 1e-23, # J/K
    # Coding for infinite Monin-Obukhov length
    InfLength = -9999
  )

  return(Constants)
}
