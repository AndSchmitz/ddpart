#Calculate wind speed from log wind profile
#This function is rearrangement of the formula for friction velocity after
#Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification and
#Eutrophication. 1995. Page 67. It is in line with https://en.wikipedia.org/wiki/Log_wind_profile
CalculateWindSpeedAtTargetHeight <- function(
  FrictionVelocity_ms,
  TargetHeight_m,
  ZeroPlaneDisplacementHeight_m,
  RoughnessLength_m,
  MoninObukhovLength_m
) {

  #Propagate NA
  if (
    is.na(FrictionVelocity_ms) |
    is.na(TargetHeight_m) |
    is.na(ZeroPlaneDisplacementHeight_m) |
    is.na(RoughnessLength_m) |
    is.na(MoninObukhovLength_m)
  ) {
    return(NA)
  }

  #Catch case TargetHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
  #This causes calculation of log(negative number) [if TargetHeight_m < ZeroPlaneDisplacementHeight_m] or
  #log(<1) which would result in a negative value of wind speed.
  if ( TargetHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m) ) {
    stop("TargetHeight_m must not be lower than (ZeroPlaneDisplacementHeight_m + RoughnessLength_m).")
 }

  #Van Karman constant is defined in helping function GetConstants()
  kappa <- GetConstants()$kappa

  #Call helping functions CalculateStabilityCorrectionForMomentum()
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

  WindSpeedAtTargetHeight_ms <- FrictionVelocity_ms / kappa * ( log((TargetHeight_m - ZeroPlaneDisplacementHeight_m)/RoughnessLength_m) - StabilityCorrectionForMomentum1 + StabilityCorrectionForMomentum2 )
  WindSpeedAtTargetHeight_ms <- round(WindSpeedAtTargetHeight_ms,GetConstants()$RoundingPrecision)

  return(WindSpeedAtTargetHeight_ms)
}
