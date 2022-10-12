#' Data for validation of dry deposition
#'
#' Data extracted from Emerson et al. (2020) fig. 1. Shows the dependency of
#' dry deposition velocity to particle size for different land use types
#' according to the old and the revised parametrization of the GEOS-Chem model.
#' Note that the old parametrization is not identical to the Zhang et al. (2001)
#' parametrization although it is termed "Zhang (2001)" in the Emerson et al.
#' (2020) paper. See ?GetLandUseParameters for details.
#'
#' @docType data
#'
#' @usage data(dd_validation)
#'
#' @format A data frame with 106 rows and 4 columns: \describe{
#'   \item{ParticleDiameter_um}{Particle diameter in um}
#'   \item{DepositionVelocity_cms}{Dry deposition velocity in cm^2/s}
#'   \item{Parametrization}{The parametrization for which the data has been
#'   extracted (GEOS-Chem old or GEOS-Chem new)}
#'   \item{LUC}{Land use class: 1 (Needleleaf
#'   forest, 2 broadleaf forest, 6 grassland)} }
#'
#' @keywords datasets
#'
#' @references Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR,
#'   McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in
#'   radiative effect estimates. Proceedings of the National Academy of Sciences
#'   2020;117:26076–26082.
#'
"dd_validation"
