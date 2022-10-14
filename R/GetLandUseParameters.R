#'@title GetLandUseParameters
#'
#'@description Get land use parameters according to Zhang et al. (2001) table 3
#'  or Emerson et al. (2020). There are 4 variants of parametrization for the
#'  Zhang01 approach:
#'
#' - A) "Zhang01": The original model according to Zhang et al. (2001). Not used
#' for the figures in Emerson et al. (2020). Implemented in ddpart as
#' parametrization "Zhang01".
#'
#' - B) "GEOS-Chem old": This is what Emerson et al. (2020) refer to as "Zhang et
#'   al. (2001)" in their Fig. 1 and 2. Implemented in ddpart as
#' parametrization "GCOld". It is an adaption of parametrization A) with two
#' differences:
#'
#'   First, the land use classes in the GEOS-Chem model differ from those in A).
#' The mapping between land use classes is not always straightforward. For
#' example, "Grassland" in B) uses parameters for "shrubs and interrupted
#'   woodland" from A). The complete mapping of LUCs is defined in file
#' "drydep_mod.F90" starting in lines 3143:
#'   https://github.com/geoschem/geos-chem/blob/main/GeosCore/drydep_mod.F90.
#'
#' For the LUCs relevant for ddpart, the mapping Emerson20 <-> Zhang01 is as
#' follows:
#'
#' - "Needleleaf" <-> "Needleleaf" (LUC1)
#'
#' - "Broadleaf" <-> "Deciduous broadleaf" (LUC4)
#'
#' - "Grassland" <-> "Shrubs and interrupted wood-lands" (LUC10)
#'
#' Second, no differentiation according to seasons exists. Instead, the average
#' over seasons 1-5 is used for each parameter. See for example
#' "drydep_mod.F90" line 3226.
#'
#' C)  "GEOS-Chem new": This is the "revised parametrization" in Emerson et al.
#' (2020) Fig. 1 and 2. Implemented in ddpart as parametrization "GCNew". It uses
#' the same assignment of LUCs as in B) and also averages land use parameters
#' over seasons, but some  parameters have been re-calibrated by Emerson et al.
#' to better match measurement data. Note that although the parameters for
#' "Shrubs and interrupted wood-lands" (LUC10 from Zhang01) are used for
#' grassland, the parametrization is valid for grassland. This is because
#' Emerson20 *calibrated* the other parameters, such that the resulting dry
#' deposition velocity matches measurement data for grassland.
#'
#' D) "GCNewSeason": Same as C) but with season-specific land use parameters.  E.g.
#' parameter "characteristic receptor radius" (A in mm) varies between seasons
#' for deciduous broadleaf forest. This is implemented in ddpart as
#' parametrization "GCNewSeason".
#'
#'
#'@param LUCNames A vector of land use class names (character). Currently,
#'  allowed values are
#'
#'  - "Grassland"
#'
#'  - "Needleleaf"
#'
#'  - "DecBroadleaf" (deciduous broadleaf)
#'
#'@param Seasons A vector of season codes (integer values 1-5).
#'
#'@param TargetParameter A character indicating which parameter to return
#'  ("z_0_m", "A_mm", "alpha" or "gamma").
#'
#'@param Parametrizations A character, one of "GCNewSeason",
#'  "Zhang01",  "GCOld", "GCNew".
#'
#'@return A vector of values for parameter "TargetParameter".
#'
#' @examples
#'
#' if (require("tidyr")) {
#'
#' GetLandUseParameters(
#'   LUCNames = c("Grassland", "Needleleaf", "DecBroadleaf"),
#'   Seasons = rep(x = 4, times = 3),
#'   Parametrizations = rep(x = "Zhang01", times = 3),
#'   TargetParameter = "A_mm"
#' )
#'
#' GetLandUseParameters(
#'   LUCNames = c("Grassland", "Needleleaf", "DecBroadleaf"),
#'   Seasons = rep(x = 4, times = 3),
#'   Parametrizations = rep(x = "GCNewSeason", times = 3),
#'   TargetParameter = "A_mm"
#' )
#'
#' }
#'
#'@export
#'
#'@import dplyr
#'
#'@references
#'
#'Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition
#'scheme for an atmospheric aerosol module. Atmospheric Environment
#'2001;35:549–560.
#'
#'Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR,
#'Farmer DK. Revisiting particle dry deposition and its role in radiative effect
#'estimates. Proceedings of the National Academy of Sciences
#'2020;117:26076–26082.


GetLandUseParameters <- function(LUCNames,
                                 Seasons,
                                 Parametrizations,
                                 TargetParameter) {

  # Sanity checks
  InputLength <- length(LUCNames)
  if (
    (length(Seasons) != InputLength) |
    (length(Parametrizations) != InputLength)
  ) {
    stop("LUCNames, Seasons and Parametrizations must be of same length.")
  }

  if (length(TargetParameter) != 1) {
    stop("TargetParameter must be of length 1.")
  }

  #For the implementation of additional LUCs: Check the mapping between Emerson
  #LUC names and Zhang LUC codes in Fortran file (see above)!
  LandUsePars <- tribble(
    ~Parametrization, ~Parameter, ~Season, ~Needleleaf, ~DecBroadleaf, ~Grassland,

    "Zhang01",         "z_0_m",        1,       0.8,        1.05,        0.1,
    "Zhang01",         "z_0_m",        2,       0.9,        1.05,        0.1,
    "Zhang01",         "z_0_m",        3,       0.9,        0.95,        0.05,
    "Zhang01",         "z_0_m",        4,       0.9,        0.55,        0.02,
    "Zhang01",         "z_0_m",        5,       0.8,        0.75,        0.05,

    "Zhang01",         "A_mm",         1,       2.0,        5.0,         2.0,
    "Zhang01",         "A_mm",         2,       2.0,        5.0,         2.0,
    "Zhang01",         "A_mm",         3,       2.0,        10.0,        5.0,
    "Zhang01",         "A_mm",         4,       2.0,        10.0,        5.0,
    "Zhang01",         "A_mm",         5,       2.0,        5.0,         2.0,

    "Zhang01",         "alpha",        1,       1.0,        0.8,         1.2,
    "Zhang01",         "alpha",        2,       1.0,        0.8,         1.2,
    "Zhang01",         "alpha",        3,       1.0,        0.8,         1.2,
    "Zhang01",         "alpha",        4,       1.0,        0.8,         1.2,
    "Zhang01",         "alpha",        5,       1.0,        0.8,         1.2,

    "Zhang01",         "gamma",        1,       0.56,       0.56,        0.54,
    "Zhang01",         "gamma",        2,       0.56,       0.56,        0.54,
    "Zhang01",         "gamma",        3,       0.56,       0.56,        0.54,
    "Zhang01",         "gamma",        4,       0.56,       0.56,        0.54,
    "Zhang01",         "gamma",        5,       0.56,       0.56,        0.54,


    "GCOld",           "A_mm",         1,       2.0,        7.0,         10.0,
    "GCOld",           "A_mm",         2,       2.0,        7.0,         10.0,
    "GCOld",           "A_mm",         3,       2.0,        7.0,         10.0,
    "GCOld",           "A_mm",         4,       2.0,        7.0,         10.0,
    "GCOld",           "A_mm",         5,       2.0,        7.0,         10.0,

    "GCOld",           "alpha",        1,       1.0,        0.8,         1.3,
    "GCOld",           "alpha",        2,       1.0,        0.8,         1.3,
    "GCOld",           "alpha",        3,       1.0,        0.8,         1.3,
    "GCOld",           "alpha",        4,       1.0,        0.8,         1.3,
    "GCOld",           "alpha",        5,       1.0,        0.8,         1.3,

    "GCOld",           "gamma",        1,       0.56,       0.56,        0.54,
    "GCOld",           "gamma",        2,       0.56,       0.56,        0.54,
    "GCOld",           "gamma",        3,       0.56,       0.56,        0.54,
    "GCOld",           "gamma",        4,       0.56,       0.56,        0.54,
    "GCOld",           "gamma",        5,       0.56,       0.56,        0.54,


    "GCNew",           "A_mm",         1,       2.0,        7.0,         10.0,
    "GCNew",           "A_mm",         2,       2.0,        7.0,         10.0,
    "GCNew",           "A_mm",         3,       2.0,        7.0,         10.0,
    "GCNew",           "A_mm",         4,       2.0,        7.0,         10.0,
    "GCNew",           "A_mm",         5,       2.0,        7.0,         10.0,

    "GCNew",           "alpha",        1,       1.0,        0.8,          1.3,
    "GCNew",           "alpha",        2,       1.0,        0.8,          1.3,
    "GCNew",           "alpha",        3,       1.0,        0.8,          1.3,
    "GCNew",           "alpha",        4,       1.0,        0.8,          1.3,
    "GCNew",           "alpha",        5,       1.0,        0.8,          1.3,

    "GCNew",           "gamma",        1,       2/3,        2/3,          2/3,
    "GCNew",           "gamma",        2,       2/3,        2/3,          2/3,
    "GCNew",           "gamma",        3,       2/3,        2/3,          2/3,
    "GCNew",           "gamma",        4,       2/3,        2/3,          2/3,
    "GCNew",           "gamma",        5,       2/3,        2/3,          2/3,


    "GCNewSeason",     "A_mm",         1,       2.0,        5.0,          10.0,
    "GCNewSeason",     "A_mm",         2,       2.0,        5.0,          10.0,
    "GCNewSeason",     "A_mm",         3,       2.0,        10.0,         10.0,
    "GCNewSeason",     "A_mm",         4,       2.0,        10.0,         10.0,
    "GCNewSeason",     "A_mm",         5,       2.0,        5.0,          10.0,

    "GCNewSeason",     "alpha",        1,       1.0,        0.8,          1.3,
    "GCNewSeason",     "alpha",        2,       1.0,        0.8,          1.3,
    "GCNewSeason",     "alpha",        3,       1.0,        0.8,          1.3,
    "GCNewSeason",     "alpha",        4,       1.0,        0.8,          1.3,
    "GCNewSeason",     "alpha",        5,       1.0,        0.8,          1.3,

    "GCNewSeason",     "gamma",        1,       2/3,        2/3,          2/3,
    "GCNewSeason",     "gamma",        2,       2/3,        2/3,          2/3,
    "GCNewSeason",     "gamma",        3,       2/3,        2/3,          2/3,
    "GCNewSeason",     "gamma",        4,       2/3,        2/3,          2/3,
    "GCNewSeason",     "gamma",        5,       2/3,        2/3,          2/3

  )

  #Convert to long format
  LandUseParsLong <- LandUsePars %>%
    pivot_longer(
      cols = -c(Parametrization, Parameter, Season),
      names_to = "LUCName",
      values_to = "Value"
    )

  #Sanity checks
  if ( !all(Seasons %in% LandUseParsLong$Season) ) {
    stop(paste("Allowed seasons are:", paste(unique(LandUseParsLong$Season), collapse = ", ")))
  }
  if ( !all(LUCNames %in% LandUseParsLong$LUCName) ) {
    stop(paste("Allowed LUCNames are:", paste(unique(LandUseParsLong$LUCName), collapse = ", ")))
  }
  if ( !all(Parametrizations %in% LandUseParsLong$Parametrization) ) {
    stop(paste("Allowed Parametrizations are:", paste(unique(LandUseParsLong$Parametrization), collapse = ", ")))
  }
  if ( !(TargetParameter %in% LandUseParsLong$Parameter) ) {
    stop(paste("Allowed parameters are:", paste(unique(LandUseParsLong$Parameter), collapse = ", ")))
  }


  #Merge input to table to extract values
  DF <- data.frame(
    Parametrization = Parametrizations,
    Season = Seasons,
    LUCName = LUCNames
  ) %>%
    mutate(
      #Order might get lost during merge()
      #Preserve!
      Order = 1:n(),
      Parameter = TargetParameter
    ) %>%
    merge(
      y = LandUseParsLong,
      by = c("Parametrization", "Season", "LUCName", "Parameter"),
      all.x = T
    ) %>%
    arrange(Order)

  if ( any(is.na(DF$Value)) ) {
    tmp <- DF %>%
      filter(
        is.na(Value)
      )
    print(tmp)
    stop("Could not find land use parameters for the combinations above.")
  }

  #Return
  return(DF$Value)
}
