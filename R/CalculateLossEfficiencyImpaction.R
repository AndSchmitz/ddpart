#' @title CalculateLossEfficiencyImpaction
#'
#' @description Calculates loss efficiency by impaction according
#' to Emerson et al. (2020) eq. 4.
#'
#' @param StokesNumber Stokes number
#'
#' @param ImpactionParameterAlpha A land-use specific empirical paramete
#'
#' @param Parametrization A character defining which parametrization to use.
#' Valid values are "Emerson20" and "Zhang01"
#'
#' @return Stokes number
#'
#' @export
#'
#' @references Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle
#' dry deposition scheme for an atmospheric aerosol module.
#' Atmospheric Environment 2001;35:549–560.

CalculateLossEfficiencyImpaction <- function(StokesNumber,
                                             ImpactionParameterAlpha,
                                             Parametrization) {

  # Sanity checks
  InputLength <- length(StokesNumber)
  if (
    (length(ImpactionParameterAlpha) != InputLength) |
      (length(Parametrization) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  C_Im <- GetParameters(Parametrization, "C_Im")
  beta <- GetParameters(Parametrization, "beta")
  E_Im <- C_Im * (StokesNumber / (ImpactionParameterAlpha + StokesNumber))^beta
  return(E_Im)
}
