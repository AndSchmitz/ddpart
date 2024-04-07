#' @title CalculateAerodynamicResistance
#'
#' @description Calculates aerodynamic resistance according to Erisman and
#'   Draaijers (1995) page 58. equation 3.4.
#'
#' @param FrictionVelocity_ms Friction velocity in in m/s.
#'
#' @param ReferenceHeight_m Reference height in m. Aerodynamic resistance will
#'   be calculated between ReferenceHeight_m and the sum of
#'   ZeroPlaneDisplacementHeight_m + RoughnessLength_m.
#'
#' @param ZeroPlaneDisplacementHeight_m Displacement height in m.
#'
#' @param RoughnessLength_m Roughness length in m.
#'
#' @param MoninObukhovLength_m Monin-Obkukhiv length in m. Monin-Obkukhiv length
#'   for neutral stratification (Pasquill class D) is "infinity". This case is
#'   encoded by a value defined in GetConstants()$InfLength in this package.
#'
#' @return Aerodynamic resistance in s/m.
#'
#' @examples
#'
#' # Aerodynamic resistance for extremely unstable stratification
#' # over grassland
#' PasquillClass <- "A"
#' RoughnessLength_m <- 0.03
#' WindSpeed_ms <- 1.5
#' AnemometerHeight_m <- 10
#' ReferenceHeight_m <- 10
#' ZeroPlaneDisplacementHeight_m <- 7 * RoughnessLength_m
#'
#' MOL_m <- CalculateMoninObukhovLength(
#'   PasquillClass = PasquillClass,
#'   RoughnessLength_m = RoughnessLength_m
#' )
#' FrictionVelocity_ms <- CalculateFrictionVelocity(
#'   WindSpeed_ms = WindSpeed_ms,
#'   AnemometerHeight_m = AnemometerHeight_m,
#'   ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeight_m,
#'   RoughnessLength_m = RoughnessLength_m,
#'   MoninObukhovLength_m = MOL_m
#' )
#' CalculateAerodynamicResistance(
#'   FrictionVelocity_ms = FrictionVelocity_ms,
#'   ReferenceHeight_m = ReferenceHeight_m,
#'   ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeight_m,
#'   RoughnessLength_m = RoughnessLength_m,
#'   MoninObukhovLength_m = MOL_m
#' )
#'
#' @export
#' @references Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to
#'   Acidification and Eutrophication. 1995.


CalculateAerodynamicResistance <- function(FrictionVelocity_ms,
                                           ReferenceHeight_m,
                                           ZeroPlaneDisplacementHeight_m,
                                           RoughnessLength_m,
                                           MoninObukhovLength_m) {

  # Sanity checks
  InputLength <- length(FrictionVelocity_ms)
  if (
    (length(ReferenceHeight_m) != InputLength) |
      (length(ZeroPlaneDisplacementHeight_m) != InputLength) |
      (length(RoughnessLength_m) != InputLength) |
      (length(MoninObukhovLength_m) != InputLength)
  ) {
    stop("All inputs must be vectors of same length.")
  }

  Dat <- data.frame(
    FrictionVelocity_ms = FrictionVelocity_ms,
    ReferenceHeight_m = ReferenceHeight_m,
    ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeight_m,
    RoughnessLength_m = RoughnessLength_m,
    MoninObukhovLength_m = MoninObukhovLength_m
  ) %>%
    mutate(
      SanityCheck = ReferenceHeight_m >= (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
    )

  # Catch case ReferenceHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
  # This causes calculation of log(negative number) [if ReferenceHeight_m < ZeroPlaneDisplacementHeight_m] or
  # log(<1) which would result in a negative Ra value.
  if (any(!Dat$SanityCheck)) {
    stop("ReferenceHeight_m must not be lower than (ZeroPlaneDisplacementHeight_m + RoughnessLength_m).")
  }


  # Von Karman constant
  kappa <- GetConstants()$kappa
  RoundingPrecision <- GetConstants()$RoundingPrecision

  Dat <- Dat %>%
    mutate(
      StabilityCorrectionForHeat_1 = CalculateStabilityCorrection(
        Numerator = (ReferenceHeight_m - ZeroPlaneDisplacementHeight_m),
        MoninObukhovLength_m = MoninObukhovLength_m,
        Type = rep("Heat", InputLength)
      ),
      StabilityCorrectionForHeat_2 = CalculateStabilityCorrection(
        Numerator = RoughnessLength_m,
        MoninObukhovLength_m = MoninObukhovLength_m,
        Type = rep("Heat", InputLength)
      ),
      # Eq. 3.4 page 58
      R_a = case_when(
        # Catch case ReferenceHeight_m == (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
        # The aerodynamic resistance between ReferenceHeight_m and (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
        # is zero by definition.
        ReferenceHeight_m == (ZeroPlaneDisplacementHeight_m + RoughnessLength_m) ~ 0,
        T ~ 1 / (kappa * FrictionVelocity_ms) * (log((ReferenceHeight_m - ZeroPlaneDisplacementHeight_m) / RoughnessLength_m) - StabilityCorrectionForHeat_1 + StabilityCorrectionForHeat_2)
      ),
      R_a = round(R_a, RoundingPrecision)
    )

  # Sanity check
  if (any(Dat$R_a < 0, na.rm = T)) {
    stop("R_a is < 0")
  }

  return(Dat$R_a)
}
