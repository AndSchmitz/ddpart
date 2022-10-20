#' @title GetParameters
#'
#' @description This function returns empirical constants for dry deposition
#'   sub-processes according to Zhang et al. (2001) or according to the
#'   re-paramtrization by Emerson et al. (2020). This function covers only
#'   parameters that are not land-use specific (see GetLandUseParameters()).
#'
#' @param Parametrization A character defining which parametrization to use.
#' See ?GetLandUseParameters for a list of valid values.
#'
#' @param TargetParameter A character indicating which parameter value to
#'   return. Valid options are "C_b", "beta", "C_Im", "nu", "C_In" and
#'   "epsilon_0". See Emerson et al. (2020) table S1.
#'
#' @return A named list of parameters.
#'
#' @examples
#' if ( require("tidyr") ) {
#'  GetParameters(Parametrization = "Zhang01", TargetParameter = "C_b")
#'  GetParameters(Parametrization = "GCNewSeason", TargetParameter = "C_b")
#' }
#'
#' @export
#'
#' @references Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR,
#' McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in
#' radiative effect estimates. Proceedings of the National Academy of Sciences
#' 2020;117:26076–26082.
#'
#' Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition
#' scheme for an atmospheric aerosol module. Atmospheric Environment
#' 2001;35:549–560.


GetParameters <- function(Parametrization,
                          TargetParameter) {

  # Sanity checks
  if (length(TargetParameter) != 1) {
    stop("TargetParameter must have length 1.")
  }

  # C_b: Empirical constants for calculation of E_b
  # beta: Empirical constants for calculation of E_Im from Emerson et al. 2020 table S1
  #C_Im: Emerson et al. 2020 table S1
  #nu: Empirical constants for calculation of E_In from Emerson et al. 2020 table S1
  #C_In: # Emerson et al. 2020 table S1
  # epsilon_0: Empirical constant for calculation of R_s from Zhang et al. 2001 eq. 5
  # also used in Emerson et al. 2020
  ParametersDF <- tribble(
    ~Parametrization, ~C_b, ~beta, ~C_Im, ~nu, ~C_In, ~epsilon_0,
    "Zhang01",          1,    2,     1,    2,    0.5,     3,
    "GCOld",            1,    2,     1,    2,    0.5,     3,
    "GCNew",            0.2,  1.7,   0.4,  0.8,  2.5,     3,
    "GCNewSeason",      0.2,  1.7,   0.4,  0.8,  2.5,     3
  )

  # Sanity checks
  if (!(all(Parametrization %in% ParametersDF$Parametrization))) {
    stop(paste("Parametrization must be on of", paste(ParametersDF$Parametrization, collapse = ",")))
  }
  ParametersDFLong <- ParametersDF %>%
    pivot_longer(
      cols = -c("Parametrization")
    )
  if ( !TargetParameter %in% ParametersDFLong$name ) {
    stop(paste("TargetParameter must be on of", paste(ParametersDFLong$name, collapse = ",")))
  }

  ParametersDFLongRelevant <- ParametersDFLong %>%
    filter(
      name == TargetParameter
    )

  Dat <- data.frame(
    Parametrization = Parametrization
  ) %>%
    mutate(
      Order = 1:n()
    ) %>%
    merge(
      y = ParametersDFLongRelevant,
      by = "Parametrization",
      all.x = T
    ) %>%
    arrange(Order)

  return(Dat$value)
}
