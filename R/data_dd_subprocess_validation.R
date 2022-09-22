#' Data for validation of dry deposition subprocesses
#'
#' Shows the dependency of the dry deposition subprocesses to particle size for
#' the parameterization according to Emerson et al. (2020) and Zhang et al.
#' (2001). Data extracted from Emerson et al. (2020) fig. 2.
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
#'   \item{Parametrization}{Parametrization (Emerson or Zhang)}
#' }
#'
#' @keywords datasets
#'
#' @references Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR,
#' McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in
#' radiative effect estimates. Proceedings of the National Academy of Sciences
#' 2020;117:26076–26082.
#'
"dd_subprocess_validation"
