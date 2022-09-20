#' @title GetLandUseParameters
#'
#' @description Get land use parameters according to Zhang et al. (2001) table 3 or Emerson et al. (2020). Emerson20 differ only in parameter "gamma" from Zhang01 regarding land-use specific parameters. The function is vectorized with respect to parameters "LUC" and "Seasons". I.e. these two parameters must be vectors of same length.
#' @param LUCs A vector of land use class codes (integer values). Currently, only land use classes 1-7 are implemented.
#' @param Seasons A vector of season codes (integer values 1-5).
#' @param TargetPar A character indicating which parameter to return from Zhang et al. (2001) table 3 ("z_0_m", "A_mm", "alpha" or "gamma").
#' @param Parametrization A character indicating which parametrization to use ("Emerson20" or "Zhang01")
#' @return A vector of values for parameter "TargetPar".
#' @examples GetLandUseParameters(LUCs = c(1, 2), Seasons = c(2, 5), TargetPar = "A_mm")
#' @export
#' @import dplyr
#' @references
#' Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.

GetLandUseParameters <- function(LUCs,
                                 Seasons,
                                 Parametrization,
                                 TargetPar) {

  # Sanity checks
  InputLength <- length(LUCs)
  if (
    (length(Seasons) != InputLength) |
      (length(Parametrization) != InputLength)
  ) {
    stop("LUCs, seasons and Parametrization must be of same length.")
  }

  if (length(TargetPar) != 1) {
    stop("TargetPar must be of length 1.")
  }

  # From Zhang et al. 2001 table 3
  # data(land_use_parameters_zhang01)
  LUCParsTable <- land_use_parameters_zhang01

  # _Sanity checks of function arguments-----
  LUCsFail <- unique(LUCs[!(LUCs %in% LUCParsTable$LUC)])
  if (length(LUCsFail) > 0) {
    stop(paste("Handling of the following land use classes is not implemented in ddpart: ", paste(LUCsFail, collapse = ",")))
  }
  SeasonsFail <- unique(Seasons[!(Seasons %in% LUCParsTable$Season)])
  if (length(SeasonsFail) > 0) {
    stop(paste("Some Seasons not valid:", paste(SeasonsFail, collapse = ",")))
  }
  if (!(TargetPar %in% c(LUCParsTable$Parameter, "SurfaceIsVegetated"))) {
    stop(paste("Parameter", TargetPar, "is not in LUCParsTable"))
  }



  # Define a function that works on one input row at a time.
  GetLandUseParameters_Scalar <- function(LUC,
                                          Season,
                                          Parametrization,
                                          TargetPar) {

    # Sanity check
    ValidParametrizations <- c("Emerson20", "Zhang01")
    if (!(Parametrization %in% ValidParametrizations)) {
      stop(paste("Parameter Parametrization must be on of", paste(ValidParametrizations, collapse = ",")))
    }

    if ((TargetPar == "gamma") & (Parametrization == "Emerson20")) {
      # Emerson et al. 2020 table S1
      return(2 / 3)
    }

    # Return already here if TargetPar == "SurfaceIsVegetated"
    NonVegetatedLUCs <- c(12:15, 8)
    if (TargetPar == "SurfaceIsVegetated") {
      if (LUC %in% NonVegetatedLUCs) {
        return(F)
      } else {
        return(T)
      }
    }


    # gamma and alpha are season-independent
    if (TargetPar %in% c("gamma", "alpha")) {
      idx <- which(
        (LUCParsTable$LUC == LUC) &
          (LUCParsTable$Parameter == TargetPar)
      )
    } else {
      idx <- which(
        (LUCParsTable$LUC == LUC) &
          (LUCParsTable$Parameter == TargetPar) &
          (LUCParsTable$Season == Season)
      )
    }
    if (length(idx) != 1) {
      stop(paste("Could not find matching row in land use parameters for LUC", LUC, TargetPar, "season", Season))
    }
    return(LUCParsTable$value[idx])

  } # end of scalar function


  # Vectorize this function
  GetLandUseParameters_Vectorized <- Vectorize(
    FUN = GetLandUseParameters_Scalar,
    USE.NAMES = F
  )

  # Call the vectorized function on the input
  ReturnValue <- GetLandUseParameters_Vectorized(
    LUC = LUCs,
    Season = Seasons,
    Parametrization,
    TargetPar
  )

  return(ReturnValue)
}
