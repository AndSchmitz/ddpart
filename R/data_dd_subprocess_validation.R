#' Data for validation of dry deposition subprocesses
#'
#' Data extracted from Emerson et al. (2020) fig. 2. Shows the dependency of
#' dry deposition subprocesses to particle size according to the old and the old
#' and the revised parametrization of the GEOS-Chem model.
#' Note that the old parametrization is not identical to the Zhang et al. (2001)
#' parametrization although it is termed "Zhang (2001)" in the Emerson et al.
#' (2020) paper. See ?GetLandUseParameters for details.
#'
#' @docType data
#'
#' @usage data(dd_subprocess_validation)
#'
#' @format A data frame with 153 rows and 4 columns:
#' \describe{
#'   \item{ParticleDiameter_um}{Particle diameter in um}
#'   \item{DepositionVelocity_cms}{Dry deposition velocity resulting from the
#'   respective dry-deposition sub-process in cm^2/s}
#'   \item{Process}{Dry-deposition sub-process}
#'   \item{Parametrization}{Parametrization (GEOS-Chem old (GCOld) or GEOS-Chem new (GCNew))}
#' }
#'
#' @keywords datasets
#'
#' @references Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR,
#'   McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in
#'   radiative effect estimates. Proceedings of the National Academy of Sciences
#'   2020;117:26076–26082.
#'
#'   Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry
#'   deposition scheme for an atmospheric aerosol module. Atmospheric
#'   Environment 2001;35:549–560.
#'
"dd_subprocess_validation"
