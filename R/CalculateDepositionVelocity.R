CalculateDepositionVelocity <- function(InputTable) {
  #Calculate vd from standard input table
  
  #Sanity-check for required column names
  RequiredColumns <- c(
    #Meteo
    "SunAngle_degree", "T_air_K", "AirPressure_Pa", "GlobalRadiation_W_m2",
    "RelHum_percent", "CloudCover_percent", "WindSpeed_AtAnemometerHeight_ms",
    "RoughnessLengthAnemometer_m", "ZeroPlaneDisplacementHeightAnemometer_m",
    "AnemometerHeight_m", "WindSpeedBlendingHeight_m",
    "Season",
    #Target land use class
    "TargetLUCCodeZhang2001", "ReferenceHeight_m",
    "RoughnessLengthTargetLUC_m", "ZeroPlaneDisplacementHeightTargetLUC",
    #Particle properties
    "GeometricMassMedianDiameter_m", "ParticleDensity_kgm3"
  )
  MissCols <- RequiredColumns[!(RequiredColumns %in% colnames(InputTable))]
  if ( length(MissCols) > 0 ) {
    stop(paste("The following columns are missing in InputTable:", paste(MissCols, sep = ",")))
  }

  #_Meteorological basics----
  Results <- InputTable %>%
    mutate(
      #Define day or night depending on sun angle
      DayOrNight = case_when(
        SunAngle_degree >= 0 ~ "Day",
        SunAngle_degree <  0 ~ "Night"
      ),
      DynamicViscosityAir = CalculateDynamicViscosityOfAir(T_air_K),
      MeanFreePathOfAirMolecule_m = CalculateMeanFreePath(
        T_air_K,
        AirPressure_Pa,
        DynamicViscosityAir
      ),
      #FIXME chaneg to 1.225 kg/m3 at standard conditions?
      #https://en.wikipedia.org/wiki/Density_of_air
      AirDensity_kgm3 = CalculateSurfaceAirDensity(
        AirPressure_Pa,
        T_air_K
      ),
      KinematicViscosityOfAir_m2s = CalculateKinematicViscosityOfAir(
        DynamicViscosityAir,
        AirDensity_kgm3
      )        
    )
  
  
  #_Wind speed at anemometer location-----
  Results <- Results %>%
    #Atmospheric stability following
    #Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification and
    #Eutrophication. 1995. Page 67.
    #and
    #Pasquill stability class
    #Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
    #2006
    rowwise() %>% #for functions that do not accept vectors as inputs
    mutate(
      PasquillClass = GetPasquillClass(
        SurfaceWindSpeed_ms = WindSpeed_AtAnemometerHeight_ms,
        DayOrNight = DayOrNight,
        IncomingSolarRadiation_Wm2 = GlobalRadiation_W_m2,
        CloudCover_percent = CloudCover_percent
      ),
      #_Friction/stability parameters for WMO grassland------
      ObukhovLength_Anemometer_m = CalculateMoninObukhovLength(
        PasquillClass = PasquillClass,
        z_0 = RoughnessLengthAnemometer_m
      ),
      FrictionVelocity_Anemometer_ms = CalculateFrictionVelocity(
        SurfaceWindSpeed_ms = WindSpeed_AtAnemometerHeight_ms,
        AnemometerHeight_m = AnemometerHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightAnemometer_m,
        RoughnessLength_m = RoughnessLengthAnemometer_m,
        MoninObukhovLength_m = ObukhovLength_Anemometer_m
      ),
      #_Wind speed at blending height------
      #FIXME Add sanity check: WS at blending height must not be << WS at anemometer height
      WindSpeedAtBlendingHeight_ms = CalculateWindSpeedAtTargetHeight(
        FrictionVelocity_ms = FrictionVelocity_Anemometer_ms,
        TargetHeight_m = WindSpeedBlendingHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightAnemometer_m,
        RoughnessLength_m = RoughnessLengthAnemometer_m,
        MoninObukhovLength_m = ObukhovLength_Anemometer_m
      )
    ) %>%
    ungroup()
  
  
  #Target-LUC dependent calculations----
  Results <- Results %>%
    mutate(
      CharacteristicRadius_m = GetLandUseParametersEmerson2020(
        LUCs = TargetLUCCodeZhang2001,
        Seasons = Season,
        TargetPar = "A"
      ) / 1e3, #convert from mm to m    
      ImpactionParameterAlpha = GetLandUseParametersEmerson2020(
        LUCs = TargetLUCCodeZhang2001,
        Seasons = Season,
        TargetPar = "alpha"
      )
    ) %>%      
    rowwise() %>% #for functions that do not accept vectors as inputs
    #_Calculate atmospheric stability----
    mutate(
      #_Friction/stability parameters-----
      ObukhovLengthTargetLUC = CalculateMoninObukhovLength(
        PasquillClass = PasquillClass,
        z_0 = RoughnessLengthTargetLUC_m
      ),
      FrictionVelocityTargetLUC_ms = CalculateFrictionVelocity(
        SurfaceWindSpeed_ms = WindSpeedAtBlendingHeight_ms,
        AnemometerHeight_m = WindSpeedBlendingHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightTargetLUC,
        RoughnessLength_m = RoughnessLengthTargetLUC_m,
        MoninObukhovLength_m = ObukhovLengthTargetLUC
      ),
      #_Aerodynamic resistance-----
      R_a_sm = CalculateAerodynamicResistance(
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        ReferenceHeight_m = ReferenceHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightTargetLUC,
        RoughnessLength_m = RoughnessLengthTargetLUC_m,
        MoninObukhovLength_m = ObukhovLengthTargetLUC
      ),
      #_Gravitational settling velocity------
      SettlingVelocity_ms = CalculateSettlingVelocity (
        ParticleDensity_kgm3 = ParticleDensity_kgm3,
        d_p_m = GeometricMassMedianDiameter_m,
        T_air_K = T_air_K,
        AirPressure_Pa = AirPressure_Pa,
        MeanFreePathOfAirMolecule_m = MeanFreePathOfAirMolecule_m,
        DynamicViscosityAir = DynamicViscosityAir
      ),
      #_Schmidt number------
      #FIXME schmidt number very large
      SchmidtNumber = CalculateSchmidtNumber(
        DynamicViscosityAir = DynamicViscosityAir,
        KinematicViscosityOfAir_m2s = KinematicViscosityOfAir_m2s,
        T_air_K = T_air_K,
        d_p_m = GeometricMassMedianDiameter_m
      ),
      #_E_b-----
      #Loss efficiency by Brownian diffusion
      E_b = CalculateLossEfficiencyBrownianDiffusion(SchmidtNumber),
      #_Stokes number-----
      StokesNumber = CalculateStokesNumber(
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        SettlingVelocity_ms = SettlingVelocity_ms,
        CharacteristicRadius_m = CharacteristicRadius_m,
        KinematicViscosityOfAir = KinematicViscosityOfAir,
        SurfaceIsVegetated = T
      ),
      #_E_Im----
      #Loss efficiency by impaction
      E_Im = CalculateLossEfficiencyImpaction(
        StokesNumber = StokesNumber,
        ImpactionParameterAlpha = ImpactionParameterAlpha
      ),
      #_E_In----
      #Loss efficiency by interception
      E_In = CalculateLossEfficiencyInterception(
        d_p_m = GeometricMassMedianDiameter_m,
        CharacteristicRadius_m = CharacteristicRadius_m
      ),
      #_R_s-----
      #Surface resistance
      SurfaceIsWet = ifelse(RelHum_percent >= 85,T,F), #FIXME
      R_s_sm = CalculateSurfaceResistance(
        SurfaceIsWet = SurfaceIsWet,
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        StokesNumber = StokesNumber,
        E_b = E_b,
        E_Im = E_Im,
        E_In = E_In
      ),
      #_V_d---------
      V_d_RefHeight_ms = SettlingVelocity_ms + 1 / (R_a_sm + R_s_sm)
    ) %>%
    ungroup()
  
  return(Results)
}
