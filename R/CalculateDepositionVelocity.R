#' @title CalculateDepositionVelocity
#'
#' @description Calculates the deposition velocity of particle according to the Zhang et al. (2001) and the Emerson et al. 2020 publications. Required inputs are:
#'
#' (1) basic meteorological data (wind speed, global ration, relative humidity, cloud cover, air temperature, air pressure, surface wetness)
#'
#' (2) information on the surface properties (land use class, roughness length and displacement height) and reference height (height of concentration measurements)
#'
#' (3) particle properties (dry particle diameter, density, optional: aerosol type to calculate hygroscopic swelling)
#'
#' The approach is designed for use cases where information on wind speed is not available directly for the target location / target land use type. For example, wind speeds are often modelled for or measured at grassland sites, but the dry deposition velocity is of interest at a close by for forest or cropland site. The approach therefore differentiates between two sites:
#'
#' - "anemometer site": Location/land use class where the wind speed measurements are available. Characterized by parameters "RoughnessLengthAnemometer_m" and "ZeroPlaneDisplacementHeightAnemometer_m"
#'
#' - "target land use site": Location/land use class where the dry deposition velocity should be calculated. Characterized by parameters "RoughnessLengthTargetLUC_m", "ZeroPlaneDisplacementHeightTargetLUC_m", "Season"
#'
#' CalculateDepositionVelocity() vertically extrapolates the wind speed at "anemometer site" the to a "blending height" (e.g. 50 m) where atmospheric conditions are somewhat independent of the underlying surface. From the blending height, the parameters relevant for particle dry deposition to the target land use site (friction velocity, aerodynamic resistance, etc.) are be calculated. The approach can also be used for cases where wind speed data is available for the target land use type directly. The main calculation steps in CalculateDepositionVelocity() are:
#'
#' 1. Calculate meteorological basics (e.g. viscosity and density of air) and whether its day of night (based on parameter SunAngle_degree)
#'
#' 2. Calculate the Pasquill class (general classification of atmospheric stability) based on data at the anemometer location (e.g. grassland land use class). This classification is also applied to the target land use type.
#'
#' 3. Calculate other meteorological parameters for the anemometer site (Obukhov length, friction velocity, stability corrections) and extrapolate wind speed to the blending height.
#'
#' - Calculate meteorological parameters for the target land use type (Obukhov length, friction velocity, stability corrections), based on the wind speed at blending height and the surface properties of the target land use type.
#'
#' - Calculate aerodynamic resistance (Ra) between the reference height and the receptor and surface resistance (Rs)
#' @param InputTable A data frame with the following columns:
#'
#'  - SunAngle_degree: The sun angle in degree. Only used to determine if its "day" (> 0°) or "night" (< 0°) which is required for the determination of the Pasquill stability class (GetPasquillClass()). The sun angle can easily be calculated with oce::sunAngle().
#'
#'  - T_air_K: Air temperature in Kelvin.
#'
#'  - AirPressure_Pa: Air pressure in Pa. Required for the calculation of the mean free path of an air molecule, which is required for the calculation of the particle settling (sedimentation) velocity.
#'
#'  - GlobalRadiation_W_m2: Global radiation in W/m2. Required for the determination of the Pasquill stability class (GetPasquillClass()).
#'
#'  - CloudCover_percent: Cloud cover in percent. Required for the determination of the Pasquill stability class (GetPasquillClass()).
#'
#'  - WindSpeedAtAnemometerHeight_ms: Wind speed (m/s) at the the anemometer height. Required for GetPasquillClass() and CalculateFrictionVelocity().
#'
#'  - RoughnessLengthAnemometer_m: Roughness length (m) of the land cover for which the anemometer wind speed is provided. Required for multiple functions.
#'
#'  - ZeroPlaneDisplacementHeightAnemometer_m: Zero plane displacement height (m) of the land cover for which the anemometer wind speed is provided. Required for multiple functions.
#'
#'  - AnemometerHeight_m:
#'
#'  - WindSpeedBlendingHeight_m:
#'
#'  - Season:
#'
#'  - TargetLUCCodeZhang2001:
#'
#'  - ReferenceHeight_m:
#'
#'  - RoughnessLengthTargetLUC_m:
#'
#'  - ZeroPlaneDisplacementHeightTargetLUC_m:
#'
#'  - DryParticleDiameter_m:
#'
#'  - ParticleDensity_kgm3:
#'
#'  AerosolType: Set parameter AerosolType to "Dry" to disable calculation of hygroscopic swelling. Set parameter "AerosolType" to one of "SeaSalt", "Urban", "Rural", "AmmoniumSulfate" to enable hygroscopic swelling (see CalculateWetRadius()).
#'
#'  - RelHum_percent:
#'
#' @return A data frame repeating the InputTable plus additional columns with calculated values.
#' @examples See vignette.
#' @export
#' @import dplyr
#' @references
#' Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.
#' Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in radiative effect estimates. Proceedings of the National Academy of Sciences 2020;117:26076–26082.


CalculateDepositionVelocity <- function(InputTable) {

  #Sanity-check for required column names
  RequiredColumns <- c(
    #Meteo
    "SunAngle_degree", "T_air_K", "AirPressure_Pa", "GlobalRadiation_W_m2",
    "RelHum_percent","CloudCover_percent", "WindSpeedAtAnemometerHeight_ms",
    "RoughnessLengthAnemometer_m", "ZeroPlaneDisplacementHeightAnemometer_m",
    "AnemometerHeight_m", "WindSpeedBlendingHeight_m", "SurfaceIsWet_bool",
    "Season",
    #Target land use class
    "TargetLUCCodeZhang2001", "ReferenceHeight_m",
    "RoughnessLengthTargetLUC_m", "ZeroPlaneDisplacementHeightTargetLUC_m",
    #Particle properties
    "DryParticleDiameter_m", "ParticleDensity_kgm3","AerosolType"
  )
  MissCols <- RequiredColumns[!(RequiredColumns %in% colnames(InputTable))]
  if ( length(MissCols) > 0 ) {
    stop(paste("The following columns are missing in InputTable:", paste(MissCols, collapse = ",")))
  }

  #Stokes number requires a classification whether the surface is
  #"vegetated" or with otherwise rough surface (Zhang et al. 2001).
  #The following LUCs are classified as non-vegetated,
  #all other are considered vegetated.
  NonVegetatedLUCs <- c(
    8, #desert
    9, #tundra
    12, #ice cap and glacier
    13, #inland water
    14 #ocean
  )

  #_Update particle radius by hygroscopic swelling-----
  Results <- InputTable %>%
    mutate(
      ParticleDiameter_m = CalculateHygroscopicSwelling(
        DryParticleDiameter_m = DryParticleDiameter_m,
        AerosolType = AerosolType,
        RelHum_percent = RelHum_percent
      )
    )

  #_Meteorological basics----
  Results <- Results %>%
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
        SurfaceWindSpeed_ms = WindSpeedAtAnemometerHeight_ms,
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
        WindSpeed_ms = WindSpeedAtAnemometerHeight_ms,
        AnemometerHeight_m = AnemometerHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightAnemometer_m,
        RoughnessLength_m = RoughnessLengthAnemometer_m,
        MoninObukhovLength_m = ObukhovLength_Anemometer_m
      ),
      #_Wind speed at blending height------
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
      CharacteristicRadius_m = GetLandUseParametersZhang2001(
        LUCs = TargetLUCCodeZhang2001,
        Seasons = Season,
        TargetPar = "A"
      ) / 1e3, #convert from mm to m
      ImpactionParameterAlpha = GetLandUseParametersZhang2001(
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
        WindSpeed_ms = WindSpeedAtBlendingHeight_ms,
        AnemometerHeight_m = WindSpeedBlendingHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightTargetLUC_m,
        RoughnessLength_m = RoughnessLengthTargetLUC_m,
        MoninObukhovLength_m = ObukhovLengthTargetLUC
      ),
      #_Aerodynamic resistance-----
      R_a_sm = CalculateAerodynamicResistance(
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        ReferenceHeight_m = ReferenceHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightTargetLUC_m,
        RoughnessLength_m = RoughnessLengthTargetLUC_m,
        MoninObukhovLength_m = ObukhovLengthTargetLUC
      ),
      #_Gravitational settling velocity------
      SettlingVelocity_ms = CalculateSettlingVelocity (
        ParticleDensity_kgm3 = ParticleDensity_kgm3,
        ParticleDiameter_m = ParticleDiameter_m,
        T_air_K = T_air_K,
        AirPressure_Pa = AirPressure_Pa,
        MeanFreePathOfAirMolecule_m = MeanFreePathOfAirMolecule_m,
        DynamicViscosityAir = DynamicViscosityAir
      ),
      #_Schmidt number------
      SchmidtNumber = CalculateSchmidtNumber(
        DynamicViscosityAir = DynamicViscosityAir,
        KinematicViscosityOfAir_m2s = KinematicViscosityOfAir_m2s,
        T_air_K = T_air_K,
        ParticleDiameter_m = ParticleDiameter_m
      ),
      #_E_b-----
      #Loss efficiency by Brownian diffusion
      E_b = CalculateLossEfficiencyBrownianDiffusion(SchmidtNumber),
      #_Stokes number-----
      SurfaceIsVegetated = case_when(
        TargetLUCCodeZhang2001 %in% NonVegetatedLUCs ~ F,
        T ~ T
      ),
      StokesNumber = CalculateStokesNumber(
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        SettlingVelocity_ms = SettlingVelocity_ms,
        CharacteristicRadius_m = CharacteristicRadius_m,
        KinematicViscosityOfAir = KinematicViscosityOfAir,
        SurfaceIsVegetated = SurfaceIsVegetated
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
        ParticleDiameter_m = ParticleDiameter_m,
        CharacteristicRadius_m = CharacteristicRadius_m
      ),
      #_R_s-----
      #Surface resistance
      R_s_sm = CalculateSurfaceResistance(
        SurfaceIsWet = SurfaceIsWet_bool,
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


  #Sanity checks-----
  Results <- Results %>%
    mutate(
      #Wind speed must be a monotonic function of height
      WindSpeedOK <- case_when(
        (AnemometerHeight_m <= WindSpeedBlendingHeight_m) & (WindSpeedAtAnemometerHeight_ms <= WindSpeedAtBlendingHeight_ms) ~ T,
        (AnemometerHeight_m >= WindSpeedBlendingHeight_m) & (WindSpeedAtAnemometerHeight_ms >= WindSpeedAtBlendingHeight_ms) ~ T,
        T ~ F
      )
    )

  if ( !all(Results$WindSpeedOK) ) {
    stop("Calculated WindSpeedAtBlendingHeight_ms is not a monotonic function of height (by comparison with WindSpeedAtAnemometerHeight_ms).")
  }

  Results <- Results %>%
    select(-WindSpeedOK)



  #Return resuts
  return(Results)
}
