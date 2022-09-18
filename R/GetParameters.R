#' @title GetParameters
#'
#' @description This function returns empirical constants for dry deposition
#' sub-processes according to Zhang et al. (2001) or according to the
#' re-paramtrization by Emerson et al. (2020). This function covers only
#' parameters that are not land-use specific (see GetLandUseParameters()).
#'
#' @return A named list of parameters.
#'
#' @examples
#' GetParameters(Parametrization = "Zhang01")
#' GetParameters(Parametrization = "Emerson20")
#'
#' @export
#' @references
#' Zhang L, He Z. Technical Note: An empirical algorithm estimating dry deposition velocity of fine, coarse and giant particles. Atmospheric Chemistry and Physics 2014;14:3729–3737.
#' Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.
#' Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in radiative effect estimates. Proceedings of the National Academy of Sciences 2020;117:26076–26082.


GetParameters <- function(Parametrization,
                          TargetParameter) {

  # Sanity checks
  if (length(TargetParameter) != 1) {
    stop("TargetParameter must have length 1.")
  }

  # Sanity check
  ValidParametrizations <- c("Emerson20", "Zhang01")
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
        Parametrization == "Emerson20" ~ 0.2,
        Parametrization == "Zhang01" ~ 1,
        T ~ NA_real_
      ),
      # Empirical constants for calculation of E_Im from Emerson et al. 2020 table S1
      beta = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Emerson20" ~ 1.7,
        Parametrization == "Zhang01" ~ 2,
        T ~ NA_real_
      ),
      C_Im = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Emerson20" ~ 0.4,
        Parametrization == "Zhang01" ~ 1,
        T ~ NA_real_
      ),
      # Empirical constants for calculation of E_In from Emerson et al. 2020 table S1
      nu = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Emerson20" ~ 0.8,
        Parametrization == "Zhang01" ~ 2,
        T ~ NA_real_
      ),
      C_In = case_when(
        # Emerson et al. 2020 table S1
        Parametrization == "Emerson20" ~ 2.5,
        Parametrization == "Zhang01" ~ 0.5,
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
