#Aerodynamic resistance
#Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification and
#Eutrophication. 1995. Page 58.

CalculateAerodynamicResistance <- function(
  FrictionVelocity_ms,
  ReferenceHeight_m,
  ZeroPlaneDisplacementHeight_m,
  RoughnessLength_m,
  MoninObukhovLength_m
) {
  
  #Propagate NA
  if (
      is.na(FrictionVelocity_ms) |
      is.na(ReferenceHeight_m) |
      is.na(ZeroPlaneDisplacementHeight_m) |
      is.na(RoughnessLength_m) |
      is.na(MoninObukhovLength_m)
  ) {
    return(NA)
  }
  
  #Catch case ReferenceHeight_m == (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
  #The aerodynamic resistance between ReferenceHeight_m and (ZeroPlaneDisplacementHeight_m + RoughnessLength_m)
  #is zero by definition.
  if ( ReferenceHeight_m == (ZeroPlaneDisplacementHeight_m + RoughnessLength_m) ) {
    return(0)
  }
  

  #Von Karman constant
  kappa <- GetConstants()$kappa

  StabilityCorrectionForHeat_1 <- CalculateStabilityCorrection(
    Numerator = (ReferenceHeight_m - ZeroPlaneDisplacementHeight_m),
    MoninObukhovLength_m = MoninObukhovLength_m,
    Type = "Heat"
  )
  
  StabilityCorrectionForHeat_2 <- CalculateStabilityCorrection(
    Numerator = RoughnessLength_m / MoninObukhovLength_m,
    MoninObukhovLength_m = MoninObukhovLength_m,
    Type = "Heat"
  )
  
  #Eq. 3.4 page 58
  R_a <- 1 / (kappa * FrictionVelocity_ms) * ( log((ReferenceHeight_m-ZeroPlaneDisplacementHeight_m)/RoughnessLength_m) - StabilityCorrectionForHeat_1 + StabilityCorrectionForHeat_2 )
  R_a <- round(R_a,GetConstants()$RoundingPrecision)
  if ( !is.na(R_a) & (R_a < 0) ) {
    stop("R_a is < 0")
  }
  
  return(R_a)
}
