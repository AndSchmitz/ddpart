#' Data for validation of dry deposition
#'
#' Shows the dependency of dry deposition velocity to particle size for
#' different land use types according to Emerson et al. (2020) and Zhang et al.
#' (2001). Data extracted from Emerson et al. (2020) fig. 1.
#'
#' @docType data
#'
#' @usage data(dd_validation)
#'
#' @format A data frame with 106 rows and 4 columns:
#' \describe{
#'   \item{ParticleDiameter_um}{Particle diameter in um}
#'   \item{DepositionVelocity_cms}{Dry deposition velocity in cm^2/s}
#'   \item{Process}{Dry-deposition sub-process}
#'   \item{LUC}{Land use class: 1 (Needleleaf forest, 2 broadleaf forest, 6
#'   grassland)}
#' }
#'
#' @keywords datasets
#'
#' @references Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR,
#' McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in
#' radiative effect estimates. Proceedings of the National Academy of Sciences
#' 2020;117:26076–26082.
#'
"dd_validation"
