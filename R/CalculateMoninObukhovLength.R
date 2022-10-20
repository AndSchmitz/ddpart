#' @title CalculateMoninObukhovLength
#'
#' @description Calculates Monin-Obukhov length according to Seinfeld and Pandis
#' (2006) page 751 eq. 16.83.
#'
#' @param PasquillClass A single character indicating the Pasquill stability class. For example according to Seinfeld and Pandis
#' (2006) page 750 as implemented in GetPasquillClass().
#'
#' @param RoughnessLength_m Roughness length in m.
#'
#' @return A numeric value for Monin-Obukhov length. Monin-Obkukhiv length for neutral stratification (Pasquill class D) is "infinity". This case is encoded by a value defined in GetConstants()$InfLength in this package.
#'
#' @examples
#'
#' # For Pasquill class A ("extremely unstable" conditions) and grassland
#' CalculateMoninObukhovLength(
#'   PasquillClass = "A",
#'   RoughnessLength_m = 0.03
#' )
#'
#' # For Pasquill class D ("neutral" conditions) and grassland
#' CalculateMoninObukhovLength(
#'   PasquillClass = "D",
#'   RoughnessLength_m = 0.03
#' )
#' GetConstants()$InfLength
#'
#' @export
#' @references Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change 2006.
#'

CalculateMoninObukhovLength <- function(PasquillClass,
                                        RoughnessLength_m) {

  # Parameters
  PasquillObukhovPars <- tribble(
    ~PasquillClass, ~a, ~b,
    "A", -0.096, 0.029,
    "B", -0.037, 0.029,
    "C", -0.002, 0.018,
    "D", 0, 0,
    "E", 0.004, -0.018,
    "F", 0.035, -0.036
  )


  # Sanity checks
  PasquillClassFail <- PasquillClass[!(PasquillClass %in% PasquillObukhovPars$PasquillClass)]
  if (length(PasquillClassFail) > 0) {
    stop(paste("Valid Pasquill classes are:", paste(PasquillObukhovPars$PasquillClass, collapse = ",")))
  }
  if (length(PasquillClass) != length(RoughnessLength_m)) {
    stop("PasquillClass and RoughnessLength_m must be vectors of same length.")
  }

  InfLength <- GetConstants()$InfLength
  RoundingPrecision <- GetConstants()$RoundingPrecision
  Dat <- data.frame(
    PasquillClass = PasquillClass,
    RoughnessLength_m = RoughnessLength_m
  ) %>%
    mutate(
      Order = 1:n()
    ) %>%
    merge(
      y = PasquillObukhovPars,
      by = "PasquillClass",
      all.x = T
    ) %>%
    mutate(
      MOL_m = case_when(
        # Monin-Obukhov-length goes to infinity in the limit of perfectly neutral
        # conditions. See for example Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation
        # to Acidification and Eutrophication. 1995. Page 66.
        # This case is coded as GetConstants()$InfLength
        (a == 0) & (b == 0) ~ InfLength,
        # SP06 eq. 16.83
        # log() in SP06 means log10 as reconstructed from example calculation given
        # immediately below SP06 eq. 16.83 (page 751)
        T ~ round(1 / (a + b * log10(RoughnessLength_m)), RoundingPrecision)
      )
    ) %>%
    arrange(Order)

  # Return
  return(Dat$MOL_m)
}
