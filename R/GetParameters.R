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
#' GetParameters(Parametrization = "Zhang01", TargetParameter = "C_b")
#' GetParameters(Parametrization = "GCNewSeason", TargetParameter = "C_b")
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

  # Sanity check
  ValidParametrizations <- c("GCNewSeason", "Zhang01", "GCOld", "GCNew")
  if (!(all(Parametrization %in% ValidParametrizations))) {
    stop(paste("Parameter Parametrization must be on of", paste(ValidParametrizations, collapse = ",")))
  }

  # Define a function that works on one input row at a time.
  GetParameters_Scalar <- function(Parametrization,
                                   TargetParameter) {
    Parameters <- list(
      # Empirical constants for calculation of E_b
      C_b = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Zhang01" ~ 1,
        Parametrization == "GCOld" ~ 1,
        Parametrization == "GCNew" ~ 0.2,
        Parametrization == "GCNewSeason" ~ 0.2,
        T ~ NA_real_
      ),
      # Empirical constants for calculation of E_Im from Emerson et al. 2020 table S1
      beta = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Zhang01" ~ 2,
        Parametrization == "GCOld" ~ 2,
        Parametrization == "GCNew" ~ 1.7,
        Parametrization == "GCNewSeason" ~ 1.7,
        T ~ NA_real_
      ),
      C_Im = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Zhang01" ~ 1,
        Parametrization == "GCOld" ~ 1,
        Parametrization == "GCNew" ~ 0.4,
        Parametrization == "GCNewSeason" ~ 0.4,
        T ~ NA_real_
      ),
      # Empirical constants for calculation of E_In from Emerson et al. 2020 table S1
      nu = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Zhang01" ~ 2,
        Parametrization == "GCOld" ~ 2,
        Parametrization == "GCNew" ~ 0.8,
        Parametrization == "GCNewSeason" ~ 0.8,
        T ~ NA_real_
      ),
      C_In = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Zhang01" ~ 0.5,
        Parametrization == "GCOld" ~ 0.5,
        Parametrization == "GCNew" ~ 2.5,
        Parametrization == "GCNewSeason" ~ 2.5,
        T ~ NA_real_
      ),
      # Empirical constant for calculation of R_s from Zhang et al. 2001 eq. 5
      # also used in Emerson et al. 2020
      epsilon_0 = 3
    )
    if (!(TargetParameter %in% names(Parameters))) {
      stop(paste("Parameter", TargetParameter, "not found."))
    }
    return(Parameters[[TargetParameter]])
  } # end of scalar function


  # Vectorize this function
  GetParameters_Vectorized <- Vectorize(
    FUN = GetParameters_Scalar,
    USE.NAMES = F
  )

  # Call the vectorized function on the input
  ReturnValue <- GetParameters_Vectorized(
    Parametrization,
    TargetParameter
  )

  return(ReturnValue)
}
