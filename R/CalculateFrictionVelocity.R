#' @title CalculateFrictionVelocity
#'
#' @description Calculates friction velocity according to Erisman and Draaijers (1995) page 67 equation 3.25.
#'
#' @param WindSpeed_ms Wind speed in m/s.
#'
#' @param ZeroPlaneDisplacementHeight_m Displacement height in m.
#'
#' @param RoughnessLength_m Roughness length in m.
#'
#' @param MoninObukhovLength_m Monin-Obkukhiv length in m. Monin-Obkukhiv length for neutral stratification (Pasquill class D) is "infinity". This case is encoded by a value defined in GetConstants()$InfLength in this package.
#'
#' @return A friction velocity value in m.
#'
#' @examples
#'
#' # Friction velocity over grassland for extremely unstable stratification
#' # (Pasquill class A)
#' MOL_m <- CalculateMoninObukhovLength(
#'   PasquillClass = "A",
#'   RoughnessLength_m = 0.03
#' )
#' CalculateFrictionVelocity(
#'   WindSpeed_ms = 1.5,
#'   AnemometerHeight_m = 10,
#'   ZeroPlaneDisplacementHeight_m = 0.21,
#'   RoughnessLength_m = 0.03,
#'   MoninObukhovLength_m = MOL_m
#' )
#'
#' # Friction velocity over grassland for neutral stratification
#' # (Pasquill class D)
#' MOL_m <- CalculateMoninObukhovLength(
#'   PasquillClass = "D",
#'   RoughnessLength_m = 0.03
#' )
#' CalculateFrictionVelocity(
#'   WindSpeed_ms = 7,
#'   AnemometerHeight_m = 10,
#'   ZeroPlaneDisplacementHeight_m = 0.21,
#'   RoughnessLength_m = 0.03,
#'   MoninObukhovLength_m = MOL_m
#' )
#' @export
#' @references Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification and Eutrophication. 1995.


CalculateFrictionVelocity <- function(WindSpeed_ms,
                                      AnemometerHeight_m,
                                      ZeroPlaneDisplacementHeight_m,
                                      RoughnessLength_m,
                                      MoninObukhovLength_m) {

  # Sanity checks
  InputLength <- length(WindSpeed_ms)
  if (
    (length(AnemometerHeight_m) != InputLength) |
      (length(ZeroPlaneDisplacementHeight_m) != InputLength) |
      (length(RoughnessLength_m) != InputLength) |
      (length(MoninObukhovLength_m) != InputLength)
  ) {
    stop("All inputs must be vectors of same length.")
  }


  # Define a function that works on one input row at a time.
  CalculateFrictionVelocity_Scalar <- function(WindSpeed_ms,
                                               AnemometerHeight_m,
                                               ZeroPlaneDisplacementHeight_m,
                                               RoughnessLength_m,
                                               MoninObukhovLength_m) {
    # Propagate NA
    if (
      is.na(WindSpeed_ms) |
        is.na(AnemometerHeight_m) |
        is.na(ZeroPlaneDisplacementHeight_m) |
        is.na(RoughnessLength_m) |
        is.na(MoninObukhovLength_m)
    ) {
      return(NA)
    }

    # Catch case AnemometerHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
    # This causes calculation of log(negative number) [if AnemometerHeight_m < ZeroPlaneDisplacementHeight_m] or
    # log(<1) which would result in a negative value of friction velocity.
    if (AnemometerHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)) {
      stop("AnemometerHeight_m must not be lower than (ZeroPlaneDisplacementHeight_m + RoughnessLength_m).")
    }


    # Van Karman constant is defined in helping function GetConstants()
    kappa <- GetConstants()$kappa

    StabilityCorrectionForMomentum1 <- CalculateStabilityCorrection(
      Numerator = (AnemometerHeight_m - ZeroPlaneDisplacementHeight_m),
      MoninObukhovLength_m = MoninObukhovLength_m,
      Type = "Momentum"
    )
    StabilityCorrectionForMomentum2 <- CalculateStabilityCorrection(
      Numerator = RoughnessLength_m,
      MoninObukhovLength_m = MoninObukhovLength_m,
      Type = "Momentum"
    )

    # log() is natural logarithm (ln()) as in publication ED95.
    FrictionVelocity_ms <- kappa * WindSpeed_ms / (log((AnemometerHeight_m - ZeroPlaneDisplacementHeight_m) / RoughnessLength_m) - StabilityCorrectionForMomentum1 + StabilityCorrectionForMomentum2)
    FrictionVelocity_ms <- round(FrictionVelocity_ms, GetConstants()$RoundingPrecision)

    if (is.na(FrictionVelocity_ms)) {
      stop("Calculation of friction velocity failed.")
    }
    return(FrictionVelocity_ms)
  } # end of CalculateFrictionVelocity_Scalar

  # Vectorize this function
  CalculateFrictionVelocity_Vectorized <- Vectorize(
    FUN = CalculateFrictionVelocity_Scalar,
    USE.NAMES = F
  )

  # Call the vectorized function on the input
  ReturnValue <- CalculateFrictionVelocity_Vectorized(
    WindSpeed_ms,
    AnemometerHeight_m,
    ZeroPlaneDisplacementHeight_m,
    RoughnessLength_m,
    MoninObukhovLength_m
  )

  # Return
  return(ReturnValue)
}
