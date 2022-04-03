#Schmidt number
#Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
#2006
#p. 574

CalculateSchmidtNumber <- function(
  DynamicViscosityAir,
  KinematicViscosityOfAir_m2s,
  T_air_K,
  ParticleDiameter_m
) {

  #Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
  #2006
  #Table 9.3 p. 407
  #Slip correction
  CunninghamCorrectionTable <- tribble(
    ~d_p_um,~CunninghamCorrection,
    0.001, 216,
    0.002, 108,
    0.005, 43.6,
    0.01,22.2,
    0.02,11.4,
    0.05,4.95,
    0.1,2.85,
    0.2,1.865,
    0.5,1.326,
    1,1.164,
    2,1.082,
    5,1.032,
    10,1.016,
    20,1.008,
    50,1.003,
    100,1.0016
  )
  d_p_um <- ParticleDiameter_m * 1e6
  if ( d_p_um < min(CunninghamCorrectionTable$d_p_um) ) {
    stop(paste("No Cunningham correction factor for particles smaller than",min(CunninghamCorrectionTable$d_p_um),"provided."))
  }
  if ( d_p_um > max(CunninghamCorrectionTable$d_p_um) ) {
    stop(paste("No Cunningham correction factor for particles larger than",max(CunninghamCorrectionTable$d_p_um),"provided."))
  }
  #Linearly interpolate Cunningham correction factor for particle size d_p_um
  idx_d_p <- which(CunninghamCorrectionTable$d_p_um == d_p_um)
  if ( length(idx_d_p) > 0 ) {
    CunninghamCorrection <- CunninghamCorrectionTable$CunninghamCorrection[idx_d_p]
  } else {
    idx_smaller_d_p_class <- max(which(CunninghamCorrectionTable$d_p_um < d_p_um))
    idx_larger_d_p_class <- idx_smaller_d_p_class + 1
    d_p_smaller <- CunninghamCorrectionTable$d_p_um[idx_smaller_d_p_class]
    d_p_larger <- CunninghamCorrectionTable$d_p_um[idx_larger_d_p_class]
    CCF_smaller <- CunninghamCorrectionTable$CunninghamCorrection[idx_smaller_d_p_class]
    CCF_larger <- CunninghamCorrectionTable$CunninghamCorrection[idx_larger_d_p_class]
    delta_d_p <- d_p_larger - d_p_smaller
    delta_CCF <- CCF_larger - CCF_smaller
    CunninghamCorrection <- CCF_smaller + delta_CCF * (d_p_um - d_p_smaller) / delta_d_p
  }


  #Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
  #2006
  #Eq. 9.73 page 416
  k <- GetConstants()$k
  BrownianDiffusivity <- k * T_air_K * CunninghamCorrection / (3 * pi * DynamicViscosityAir * ParticleDiameter_m)

  #Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
  #2006
  #p. 574
  SchmidtNumber <- KinematicViscosityOfAir_m2s / BrownianDiffusivity
  return(SchmidtNumber)
}
