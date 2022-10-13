#' @title CalculateLossEfficiencyBrownianDiffusion
#'
#' @description Calculates loss efficiency by brownian diffusion according
#' to Emerson et al. (2020) eq. 3.
#'
#' @param SchmidtNumber Schmidt number. E.g. provided by
#' CalculateSchmidtNumber()
#'
#' @param BrownianDiffusionParameterGamma Empirical parameter. Land use specific
#' in Zhang et al. (2001) and constant at a value of 2/3 in Emerson et al.
#' (2020)
#'
#' @param Parametrization A character defining which parametrization to use.
#' See ?GetLandUseParameters for a list of valid values.
#'
#' @return Loss efficiency by brownian diffusion
#'
#' @export
#' @references Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR,
#' McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in
#' radiative effect estimates. Proceedings of the National Academy of Sciences
#' 2020;117:26076–26082.


CalculateLossEfficiencyBrownianDiffusion <- function(SchmidtNumber,
                                                     BrownianDiffusionParameterGamma,
                                                     Parametrization) {

  # Sanity checks
  InputLength <- length(SchmidtNumber)
  if (
    (length(BrownianDiffusionParameterGamma) != InputLength) |
      (length(Parametrization) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  C_b <- GetParameters(Parametrization, "C_b")
  E_b <- C_b * SchmidtNumber^(-BrownianDiffusionParameterGamma)
  return(E_b)
}
