#' @title CalculateWindSpeedAtTargetHeight
#'
#' @description Calculate wind speed according to stability-corrected log wind
#' profile. This function is rearrangement of the formula for friction velocity
#' after Erisman and Draaijers (1995) page 67. It is in line with
#' https://en.wikipedia.org/wiki/Log_wind_profile
#'
#' @param FrictionVelocity_ms Friction velocity in m/s.
#'
#' @param TargetHeight_m Height at which the wind speed should be calculated in
#' m.
#'
#' @param ZeroPlaneDisplacementHeight_m Zero-plane displacement height in m.
#'
#' @param RoughnessLength_m Roughness length in m.
#'
#' @param MoninObukhovLength_m Monin-Obukhov-length in m.
#'
#' @return Wind speed at target height in m/s.
#'
#' @export
#'
#' @references Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification
# and Eutrophication. 1995

CalculateWindSpeedAtTargetHeight <- function(FrictionVelocity_ms,
                                             TargetHeight_m,
                                             ZeroPlaneDisplacementHeight_m,
                                             RoughnessLength_m,
                                             MoninObukhovLength_m) {


  # Sanity checks
  InputLength <- length(FrictionVelocity_ms)
  if (
    (length(TargetHeight_m) != InputLength) |
    (length(ZeroPlaneDisplacementHeight_m) != InputLength) |
    (length(RoughnessLength_m) != InputLength) |
    (length(MoninObukhovLength_m) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  # Define a function that works on one input row at a time.
  CalculateWindSpeedAtTargetHeight_Scalar <- function(FrictionVelocity_ms,
                                                      TargetHeight_m,
                                                      ZeroPlaneDisplacementHeight_m,
                                                      RoughnessLength_m,
                                                      MoninObukhovLength_m) {

    # Propagate NA
    if (
      is.na(FrictionVelocity_ms) |
      is.na(TargetHeight_m) |
      is.na(ZeroPlaneDisplacementHeight_m) |
      is.na(RoughnessLength_m) |
      is.na(MoninObukhovLength_m)
    ) {
      return(NA)
    }

    # Catch case TargetHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
    # This causes calculation of log(negative number) [if TargetHeight_m < ZeroPlaneDisplacementHeight_m] or
    # log(<1) which would result in a negative value of wind speed.
    if (TargetHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)) {
      stop("TargetHeight_m must not be lower than (ZeroPlaneDisplacementHeight_m + RoughnessLength_m).")
    }

    # Van Karman constant is defined in helping function GetConstants()
    kappa <- GetConstants()$kappa

    # Call helping functions CalculateStabilityCorrectionForMomentum()
    StabilityCorrectionForMomentum1 <- CalculateStabilityCorrection(
      Numerator = (TargetHeight_m - ZeroPlaneDisplacementHeight_m),
      MoninObukhovLength_m = MoninObukhovLength_m,
      Type = "Momentum"
    )
    StabilityCorrectionForMomentum2 <- CalculateStabilityCorrection(
      Numerator = RoughnessLength_m,
      MoninObukhovLength_m = MoninObukhovLength_m,
      Type = "Momentum"
    )

    WindSpeedAtTargetHeight_ms <- FrictionVelocity_ms / kappa * (log((TargetHeight_m - ZeroPlaneDisplacementHeight_m) / RoughnessLength_m) - StabilityCorrectionForMomentum1 + StabilityCorrectionForMomentum2)
    WindSpeedAtTargetHeight_ms <- round(WindSpeedAtTargetHeight_ms, GetConstants()$RoundingPrecision)

    return(WindSpeedAtTargetHeight_ms)
  } # end of scalar function


  # Vectorize this function
  CalculateWindSpeedAtTargetHeight_Vectorized <- Vectorize(
    FUN = CalculateWindSpeedAtTargetHeight_Scalar,
    USE.NAMES = F
  )

  # Call the vectorized function on the input
  ReturnValue <- CalculateWindSpeedAtTargetHeight_Vectorized(
    FrictionVelocity_ms,
    TargetHeight_m,
    ZeroPlaneDisplacementHeight_m,
    RoughnessLength_m,
    MoninObukhovLength_m
  )

  return(ReturnValue)
}
