#Friction velocity
#Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification and
#Eutrophication. 1995. Page 67.

CalculateFrictionVelocity <- function(
  WindSpeed_ms,
  AnemometerHeight_m,
  ZeroPlaneDisplacementHeight_m,
  RoughnessLength_m,
  MoninObukhovLength_m
) {

  #Propagate NA
  if (
    is.na(WindSpeed_ms) |
    is.na(AnemometerHeight_m) |
    is.na(ZeroPlaneDisplacementHeight_m) |
    is.na(RoughnessLength_m) |
    is.na(MoninObukhovLength_m)
  ) {
    return(NA)
  }

  #Catch case AnemometerHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
  #This causes calculation of log(negative number) [if AnemometerHeight_m < ZeroPlaneDisplacementHeight_m] or
  #log(<1) which would result in a negative value of friction velocity.
  if ( AnemometerHeight_m < (ZeroPlaneDisplacementHeight_m + RoughnessLength_m) ) {
    stop("AnemometerHeight_m must not be lower than (ZeroPlaneDisplacementHeight_m + RoughnessLength_m).")
  }


  #Van Karman constant is defined in helping function GetConstants()
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

  #log() is natural logarithm (ln()) as in publication ED95.
  FrictionVelocity_ms <- kappa * WindSpeed_ms / ( log((AnemometerHeight_m - ZeroPlaneDisplacementHeight_m)/RoughnessLength_m) - StabilityCorrectionForMomentum1 + StabilityCorrectionForMomentum2 )
  FrictionVelocity_ms <- round(FrictionVelocity_ms,GetConstants()$RoundingPrecision)

  if ( is.na(FrictionVelocity_ms) ) {
    stop("Calculation of friction velocity failed.")
  }
  return(FrictionVelocity_ms)
}
