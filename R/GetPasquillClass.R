#' @title GetPasquillClass
#'
#' @description Calculates Pasquill stability class according to Seinfeld and
#' Pandis (2006) page 750.
#'
#' @param SurfaceWindSpeed_ms Wind speed at surface in m/s.
#'
#' @param DayOrNight Either "Day" or "Night" (character)
#'
#' @param IncomingSolarRadiation_Wm2 Solar radiation in W/m2.
#'
#' @param CloudCover_percent Cloud cover in percent.
#'
#' @return Chracter string indicating the Pasquill stability class (A - F)
#'
#' @export
#'
#' @references Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From
#' Air Pollution to Climate Change. 2006.



GetPasquillClass <- function(SurfaceWindSpeed_ms,
                             DayOrNight,
                             IncomingSolarRadiation_Wm2,
                             CloudCover_percent) {

  # Sanity checks
  InputLength <- length(SurfaceWindSpeed_ms)
  if (
    (length(DayOrNight) != InputLength) |
      (length(IncomingSolarRadiation_Wm2) != InputLength) |
      (length(CloudCover_percent) != InputLength)
  ) {
    stop("All inputs must be vectors of same length.")
  }
  if (!(all(DayOrNight %in% c("Day", "Night")))) {
    stop(paste("Error in function PasquillClass(): Argument \"DayOrNight\" must have value \"Day\" or \"Night\" and not value:", DayOrNight))
  }

  Dat <- data.frame(
    SurfaceWindSpeed_ms = SurfaceWindSpeed_ms,
    DayOrNight = DayOrNight,
    IncomingSolarRadiation_Wm2 = IncomingSolarRadiation_Wm2,
    CloudCover_percent = CloudCover_percent
  ) %>%
    mutate(
      Order = 1:n(),
      WindSpeedCategory = dplyr::case_when(
        SurfaceWindSpeed_ms < 2 ~ 1,
        SurfaceWindSpeed_ms < 3 ~ 2,
        SurfaceWindSpeed_ms < 5 ~ 3,
        SurfaceWindSpeed_ms < 6 ~ 4,
        T ~ 5
      ),
      RadiationCategory = dplyr::case_when(
        IncomingSolarRadiation_Wm2 < 350 ~ "Slight",
        IncomingSolarRadiation_Wm2 < 700 ~ "Moderate",
        T ~ "Strong"
      ),
      CloudCoverFraction = dplyr::case_when(
        CloudCover_percent >= 50 ~ "High",
        T ~ "Low"
      )
    )

  ClassificationTableDayTime <- tribble(
    ~WindSpeedCategory, ~RadiationCategory, ~StabClassDay,
    1, "Strong", "A",
    1, "Moderate", "A",
    1, "Slight", "B",
    2, "Strong", "A",
    2, "Moderate", "B",
    2, "Slight", "C",
    3, "Strong", "B",
    3, "Moderate", "B",
    3, "Slight", "C",
    4, "Strong", "C",
    4, "Moderate", "C",
    4, "Slight", "D",
    5, "Strong", "C",
    5, "Moderate", "D",
    5, "Slight", "D"
  )

  ClassificationTableNightTime <- tribble(
    ~WindSpeedCategory, ~CloudCoverFraction, ~StabClassNight,
    1, "High", "E", # Missing in SP06, filled based on https://www.ready.noaa.gov/READYpgclass.php
    1, "Low", "F", # Missing in SP06, filled based on https://www.ready.noaa.gov/READYpgclass.php
    2, "High", "E",
    2, "Low", "F",
    3, "High", "D",
    3, "Low", "E",
    4, "High", "D",
    4, "Low", "D",
    5, "High", "D",
    5, "Low", "D"
  )

  Dat <- Dat %>%
    merge(
      y = ClassificationTableNightTime,
      all.x = T
    ) %>%
    merge(
      y = ClassificationTableDayTime,
      all.x = T
    ) %>%
    mutate(
      PasquillClass = case_when(
        # Neutral category D must be used regardless of wind speed for overcast conditions during day or night.
        # "Overcast" is in case of >= 95% cloud cover (https://en.wikipedia.org/wiki/Overcast)
        CloudCover_percent >= 95 ~ "D",
        # In case of ambiguous classes in SP06 (e.g. "A-B"), the lower class is selected.
        DayOrNight == "Day" ~ StabClassDay,
        DayOrNight == "Night" ~ StabClassNight,
        T ~ "ERROR"
      )
    ) %>%
    arrange(Order)

  if (any(Dat$PasquillClass == "ERROR")) {
    stop("Error assigning Pasquill classes")
  }

  return(Dat$PasquillClass)
}
