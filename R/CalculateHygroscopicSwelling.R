#' @title CalculateHygroscopicSwelling
#'
#' @description Calculates increase in particle diameter due to water update
#' according to Zhang et al. (2001) eq. 10 or Gerber (1985), see Details.
#' All parameters must be vectors of same lengths.
#'
#' @param DryParticleDiameter_m The diameter of the particles before application
#' of hygroscopic swelling (dry) in m.
#'
#' @param AerosolType Hygroscopic swelling differs depending on aerosol type.
#' Implemented types are (1) "Dry" for no swelling, (2) "SeaSalt", (3)  "Urban",
#' (4) "Rural" and (5) "AmmoniumSulfate".
#'
#' @param RelHum_percent Relative humidity in percent.
#'
#' @param UseCorrectedFormula A boolean indicating whether to use the
#' (potentially wrong) formulation of hygroscopic swelling according to eq. 10
#' in Zhang et al. (2001) (UseCorrectedFormula = FALSE) or whether to use the
#' original formulation according to Gerber 1985 eq. 15 and 22
#' (UseCorrectedFormula = TRUE). See Details.
#'
#' @details Zhang et al. (2001) provides Gerber (1985) as reference for the
#' formulation of hygroscopic swelling (eq. 10). However, the formulations in
#' Zhang et al. (2001) eq. 10 and Gerber (1985) (eq. 15 and 22) are not equal.
#' Gerber (1985) raises the whole equation used by Zhang et al. (2001) to the
#' power of 1/3. Therefore, if "UseCorrectedFormula = TRUE", the whole
#' expression from Zhang et al. (2001) is raised to the power of 1/3.
#'
#' @return A vector of particle diameters after accounting for hygroscopic
#' swelling in m.
#'
#' @export
#'
#' @import dplyr
#'
#' @references
#'
#'  - Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition
#' scheme for an atmospheric aerosol module. Atmospheric Environment
#' 2001;35:549–560.
#'
#' - Gerber HE. 1985. Relative - Humidity Parameterization of the Navy Aerosol
#'Model (NAM). NAVAL RESEARCH LAB WASHINGTON DC; December 30, 1985. Available
#'at: https://apps.dtic.mil/sti/citations/ADA163209.


CalculateHygroscopicSwelling <- function(DryParticleDiameter_m,
                                         AerosolType,
                                         RelHum_percent,
                                         UseCorrectedFormula) {

  # Sanity checks
  InputLength <- length(DryParticleDiameter_m)
  if (
    (length(AerosolType) != InputLength) |
      (length(RelHum_percent) != InputLength) |
      (length(UseCorrectedFormula) != InputLength)
  ) {
    stop("All inputs must be vectors of same length.")
  }

  # Zhang et al. 2001 Table 1:
  HygroscopicSwellingPars <- tribble(
    ~AerosolType, ~C1, ~C2, ~C3, ~C4,
    "SeaSalt", 0.7674, 3.079, 2.573 * 1e-11, -1.424,
    "Urban", 0.3926, 3.101, 4.190 * 1e-11, -1.404,
    "Rural", 0.2789, 3.115, 5.415 * 1e-11, -1.399,
    "AmmoniumSulfate", 0.4809, 3.082, 3.110 * 1e-11, -1.428,
    # extra row to disable hygroscopic swelling
    "Dry", -9999, -9999, -9999, -9999
  )

  if (!(all(AerosolType %in% c("Dry", HygroscopicSwellingPars$AerosolType)))) {
    stop(paste("All values for parameter AerosolType must be in ", paste(unique(HygroscopicSwellingPars$AerosolType, collapse = ","))))
  }

  Data <- data.frame(
    DryRadius_m = DryParticleDiameter_m / 2,
    AerosolType = AerosolType,
    RelHum_percent = RelHum_percent
  ) %>%
    mutate(
      order = 1:n()
    ) %>%
    merge(
      y = HygroscopicSwellingPars,
      by = "AerosolType"
    ) %>%
    mutate(
      # While eq. 10 in Zhang et al. 2001 includes the brackets around the whole
      # equation, it misses the power of (1/3) at the end, which is present in
      #the original publication (Gerber 1985 eq. 15 and 22).
      ValueZhang = (C1 * DryRadius_m^C2) / (C3 * DryRadius_m^C4 - log(RelHum_percent / 100)) + DryRadius_m^3,
      ValueGerber = ValueZhang^(1/3),
      WetRadius_m = case_when(
        # No change if AerosolType == "Dry"
        AerosolType == "Dry" ~ DryRadius_m,
        (AerosolType != "Dry") & (UseCorrectedFormula) ~ T ~ ValueZhang,
        (AerosolType != "Dry") & (!UseCorrectedFormula) ~ T ~ ValueGerber,
        T ~ NA_real_
      ),
      WetDiameter_m = WetRadius_m * 2
    ) %>%
    arrange(order)

  return(Data$WetDiameter_m)
}
