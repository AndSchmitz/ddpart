#' Land-use specific parameters according to Zhang et al. (2001) table 3.
#'
#' Note that parameter A (characteristic radius of receptor surface) comes in mm
#' and not in m. Note that the Emerson et al. (2020) parametrization uses a
#' different value for parameter gamma. Season 999 indicates that the value
#' applies to all seasons.
#'
#' @docType data
#'
#' @usage data(land_use_parameters_zhang01)
#'
#' @format A data frame with 120 rows and 4 columns:
#' \describe{
#'   \item{Parameter}
#'   \item{Season}
#'   \item{LUC} {Land use class}
#'   \item{value}
#' }
#'
#' @keywords datasets
#'
#' @references Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle
#' dry deposition scheme for an atmospheric aerosol module. Atmospheric
#' Environment 2001;35:549–560.
#'
#' Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR,
#' Farmer DK. Revisiting particle dry deposition and its role in radiative
#' effect estimates. Proceedings of the National Academy of Sciences
#' 2020;117:26076–26082.


"vd_windspeed_validation_zhang01"
