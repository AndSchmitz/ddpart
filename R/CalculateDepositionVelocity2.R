#' @title CalculateDepositionVelocity2
#'
#' @description Calculates the deposition velocity of particle according to the
#'   Zhang et al. (2001) and the Emerson et al. (2020) publications.
#'
#'   This is a wrapper function over the various dry deposition sub-processes.
#'   It is designed for use cases where information on wind speed is
#'   available for one land use but dry deposition velocities should be
#'   calculated for another land use. For example, wind speed is available from
#'   modelled data for standard WMO conditions (10 m over grassland) but the
#'   dry deposition velocity should be calculated for forest. Use
#'   CalculateDepositionVelocity() for the more typical case where wind speed
#'   information is available for the sane land use type where dry deposition
#'   should be calculated.
#'
#'   The approach therefore differentiates between two sites:
#'
#'   - "anemometer site": Location/land use class where the wind speed
#'   measurements are available. Characterized by parameters
#'   "RoughnessLengthAnemometer_m" and "ZeroPlaneDisplacementHeightAnemometer_m"
#'
#'   - "target land use site": Location/land use class where the dry deposition
#'   velocity should be calculated. Characterized by parameters
#'   "RoughnessLengthTargetLUC_m", "ZeroPlaneDisplacementHeightTargetLUC_m",
#'   "Season"
#'
#'   CalculateDepositionVelocity2() vertically extrapolates the wind speed at
#'   "anemometer site" the to a "blending height" (e.g. 50 m) where atmospheric
#'   conditions are somewhat independent of the underlying surface. From the
#'   blending height, the parameters relevant for particle dry deposition to the
#'   target land use site (friction velocity, aerodynamic resistance, etc.) are
#'   be calculated. The main calculation steps in CalculateDepositionVelocity()
#'   are:
#'
#'   1. Calculate meteorological basics (e.g. viscosity and density of air) and
#'   whether its day of night (based on parameter SunAngle_degree)
#'
#'   2. Calculate the Pasquill class (general classification of atmospheric
#'   stability) based on data at the anemometer location (e.g. grassland land
#'   use class). This classification is also applied to the target land use
#'   type.
#'
#'   3. Calculate other meteorological parameters for the anemometer site
#'   (Obukhov length, friction velocity, stability corrections) and extrapolate
#'   wind speed to the blending height, using CalculateWindSpeedAtTargetHeight()
#'
#'   4. Calculate meteorological parameters for the target land use type (Obukhov
#'   length, friction velocity, stability corrections), based on the wind speed
#'   at blending height and the surface properties of the target land use type.
#'
#'   5. Calculate the aerodynamic resistance (Ra) between the reference height
#'   and the effective height of the receptor / target land use type. Calculate
#'   surface resistance (Rs) and gravitation settling.
#'
#'   6. Calculate dry deposition velocity to the target land use class.
#'
#' @param InputTable A data frame with the all columns required by
#' CalculateDepositionVelocity(), but:
#'
#'   - RoughnessLengthAnemometer_m: Roughness length (m) of the land cover for
#'   which the anemometer wind speed is provided. Required for multiple
#'   functions.
#'
#'   - ZeroPlaneDisplacementHeightAnemometer_m: Zero plane displacement height
#'   (m) of the land cover for which the anemometer wind speed is provided.
#'   Required for multiple functions.
#'
#'   - WindSpeedBlendingHeight_m: Height in m to which the wind speed is
#'   extrapolated. At this height, the wind speed is assumed to be independent
#'   of the underlying land use.
#'
#'   - TargetLUCNames: Land use classes (character). See ?GetLandUseParameters for
#'   information.
#'
#'   - RoughnessLengthTargetLUC_m: Roughness length (m) of the land cover for
#'   which the dry deposition velocities are calculated.
#'
#'   - ZeroPlaneDisplacementHeightTargetLUC_m: Zero plane displacement height
#'   (m) of the land cover for which the dry deposition velocities are
#'   calculated.
#'
#' @return A data frame repeating the InputTable plus additional columns with
#'   calculated values.
#'
#' @examples
#' # Define some standard conditions
#' Basics <- data.frame(
#'   SunAngle_degree = 30,
#'   T_air_K = 293.15,
#'   AirPressure_Pa = 101325,
#'   SurfaceIsWet_bool = FALSE,
#'   GlobalRadiation_W_m2 = 100,
#'   RelHum_percent = 80,
#'   CloudCover_percent = 80,
#'   RoughnessLengthAnemometer_m = 0.03, # WMO grassland
#'   ZeroPlaneDisplacementHeightAnemometer_m = 0.21,
#'   AnemometerHeight_m = 10,
#'   WindSpeedAtAnemometerHeight_ms = 5,
#'   WindSpeedBlendingHeight_m = 50, # WS no longer dependent on LUC at 50m
#'   Season = 1,
#'   DryParticleDiameter_m = 5e-6,
#'   ParticleDensity_kgm3 = 2000,
#'   AerosolType = "Dry", # disable hygroscopic swelling
#'   Parametrization = "Emerson20",
#'   ReferenceHeight_m = 50 # assuming concentrations are known at blending height
#' )
#'
#' # Define two land use classes for which to calculate dry deposition velocity
#' TargetLUCs <- data.frame(
#'   # 1=evegreen needleleaf forest, 2=evergreen broadleaf forest
#'   TargetLUCNames = c(1, 2),
#'   RoughnessLengthTargetLUC_m = c(0.8, 2.65) # Zhang01 table 3
#' ) %>%
#'   mutate(
#'     ZeroPlaneDisplacementHeightTargetLUC_m = 7 * RoughnessLengthTargetLUC_m
#'   )
#'
#' # Run calculations
#' InputTable <- merge(Basics, TargetLUCs)
#' Output <- CalculateDepositionVelocity2(
#'   InputTable = InputTable
#' )
#'
#' # Show results. LUC 2 (evergreen broadleaf forest) has a higher vd compared to
#' # LUC 1 (evegreen needleleaf forest) for the specific settings used.
#' print(Output %>% select(TargetLUCNames, V_d_RefHeight_ms))
#'
#' @export
#'
#' @import dplyr
#'
#' @references Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle
#' dry deposition scheme for an atmospheric aerosol module. Atmospheric
#' Environment 2001;35:549–560. Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR,
#' Pierce JR, McMeeking GR, Farmer DK. Revisiting particle dry deposition and
#' its role in radiative effect estimates. Proceedings of the National Academy
#' of Sciences 2020;117:26076–26082.


CalculateDepositionVelocity2 <- function(InputTable) {

  # Sanity-check for required column names
  RequiredColumns <- c(
    # Meteo
    "SunAngle_degree", "T_air_K", "AirPressure_Pa", "GlobalRadiation_W_m2",
    "RelHum_percent", "CloudCover_percent", "WindSpeedAtAnemometerHeight_ms",
    "RoughnessLengthAnemometer_m", "ZeroPlaneDisplacementHeightAnemometer_m",
    "AnemometerHeight_m", "WindSpeedBlendingHeight_m", "SurfaceIsWet_bool",
    "Season",
    # Target land use class
    "TargetLUCNames", "ReferenceHeight_m",
    "RoughnessLengthTargetLUC_m", "ZeroPlaneDisplacementHeightTargetLUC_m",
    # Particle properties
    "DryParticleDiameter_m", "ParticleDensity_kgm3", "AerosolType",
    "Parametrization"
  )
  MissCols <- RequiredColumns[!(RequiredColumns %in% colnames(InputTable))]
  if (length(MissCols) > 0) {
    stop(paste("The following columns are missing in InputTable:", paste(MissCols, collapse = ",")))
  }

  # Stokes number requires a classification whether the surface is
  # "vegetated" or with otherwise rough surface (Zhang et al. 2001).
  # The following LUCs are classified as non-vegetated,
  # all other are considered vegetated.
  NonVegetatedLUCs <- c(
    8, # desert
    9, # tundra
    12, # ice cap and glacier
    13, # inland water
    14 # ocean
  )

  # Update particle radius by hygroscopic swelling-----
  Results <- InputTable %>%
    mutate(
      ParticleDiameter_m = CalculateHygroscopicSwelling(
        DryParticleDiameter_m = DryParticleDiameter_m,
        AerosolType = AerosolType,
        RelHum_percent = RelHum_percent
      )
    )

  # Meteorological basics----
  Results <- Results %>%
    mutate(
      # Define day or night depending on sun angle
      DayOrNight = case_when(
        SunAngle_degree >= 0 ~ "Day",
        SunAngle_degree < 0 ~ "Night"
      ),
      DynamicViscosityAir_kgms = CalculateDynamicViscosityOfAir(T_air_K),
      MeanFreePathOfAirMolecule_m = CalculateMeanFreePath(
        T_air_K,
        AirPressure_Pa,
        DynamicViscosityAir_kgms
      ),
      AirDensity_kgm3 = CalculateAirDensity(
        AirPressure_Pa,
        T_air_K
      ),
      KinematicViscosityOfAir_m2s = CalculateKinematicViscosityOfAir(
        DynamicViscosityAir_kgms,
        AirDensity_kgm3
      )
    )

  # Wind speed at blending height-----
  Results <- Results %>%
    mutate(
      PasquillClass = GetPasquillClass(
        SurfaceWindSpeed_ms = WindSpeedAtAnemometerHeight_ms,
        DayOrNight = DayOrNight,
        IncomingSolarRadiation_Wm2 = GlobalRadiation_W_m2,
        CloudCover_percent = CloudCover_percent
      ),
      # _Friction/stability parameters for anemometer land use-----
      MoninObukhovLength_Anemometer_m = CalculateMoninObukhovLength(
        PasquillClass = PasquillClass,
        RoughnessLength_m = RoughnessLengthAnemometer_m
      ),
      FrictionVelocity_Anemometer_ms = CalculateFrictionVelocity(
        WindSpeed_ms = WindSpeedAtAnemometerHeight_ms,
        AnemometerHeight_m = AnemometerHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightAnemometer_m,
        RoughnessLength_m = RoughnessLengthAnemometer_m,
        MoninObukhovLength_m = MoninObukhovLength_Anemometer_m
      ),
      # _Wind speed at blending height------
      WindSpeedAtBlendingHeight_ms = CalculateWindSpeedAtTargetHeight(
        FrictionVelocity_ms = FrictionVelocity_Anemometer_ms,
        TargetHeight_m = WindSpeedBlendingHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightAnemometer_m,
        RoughnessLength_m = RoughnessLengthAnemometer_m,
        MoninObukhovLength_m = MoninObukhovLength_Anemometer_m
      )
    )

  # Target-LUC dependent calculations----
  Results <- Results %>%
    mutate(
      CharacteristicRadius_m = 0.001 * GetLandUseParameters(
        LUCs = TargetLUCNames,
        Seasons = Season,
        TargetPar = "A_mm",
        Parametrization = Parametrization
      ),
      ImpactionParameterAlpha = GetLandUseParameters(
        LUCs = TargetLUCNames,
        Seasons = Season,
        TargetPar = "alpha",
        Parametrization = Parametrization
      )
    ) %>%
    # _Atmospheric stability----
    mutate(
      # _Friction/stability parameters-----
      MoninObukhovLengthTargetLUC = CalculateMoninObukhovLength(
        PasquillClass = PasquillClass,
        RoughnessLength_m = RoughnessLengthTargetLUC_m
      ),
      FrictionVelocityTargetLUC_ms = CalculateFrictionVelocity(
        WindSpeed_ms = WindSpeedAtBlendingHeight_ms,
        AnemometerHeight_m = WindSpeedBlendingHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightTargetLUC_m,
        RoughnessLength_m = RoughnessLengthTargetLUC_m,
        MoninObukhovLength_m = MoninObukhovLengthTargetLUC
      ),
      # _Aerodynamic resistance-----
      R_a_sm = CalculateAerodynamicResistance(
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        ReferenceHeight_m = ReferenceHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeightTargetLUC_m,
        RoughnessLength_m = RoughnessLengthTargetLUC_m,
        MoninObukhovLength_m = MoninObukhovLengthTargetLUC
      ),
      # _Gravitational settling velocity------
      MeanFreePathOfAirMolecule_m = CalculateMeanFreePath(
        T_air_K = T_air_K,
        AirPressure_Pa = AirPressure_Pa,
        DynamicViscosityAir_kgms = DynamicViscosityAir_kgms
      ),
      SettlingVelocity_ms = CalculateSettlingVelocity(
        ParticleDensity_kgm3 = ParticleDensity_kgm3,
        ParticleDiameter_m = ParticleDiameter_m,
        # T_air_K = T_air_K,
        # AirPressure_Pa = AirPressure_Pa,
        MeanFreePathOfAirMolecule_m = MeanFreePathOfAirMolecule_m,
        DynamicViscosityAir_kgms = DynamicViscosityAir_kgms
      ),
      # _Schmidt number------
      SchmidtNumber = CalculateSchmidtNumber(
        DynamicViscosityAir_kgms = DynamicViscosityAir_kgms,
        KinematicViscosityOfAir_m2s = KinematicViscosityOfAir_m2s,
        T_air_K = T_air_K,
        ParticleDiameter_m = ParticleDiameter_m
      ),
      # _E_b-----
      # Loss efficiency by Brownian diffusion
      BrownianDiffusionParameterGamma = GetLandUseParameters(
        LUCs = TargetLUCNames,
        Seasons = Season,
        TargetPar = "gamma",
        Parametrization = Parametrization
      ),
      E_b = CalculateLossEfficiencyBrownianDiffusion(
        SchmidtNumber = SchmidtNumber,
        BrownianDiffusionParameterGamma = BrownianDiffusionParameterGamma,
        Parametrization = Parametrization
      ),
      # _Stokes number-----
      SurfaceIsVegetated = case_when(
        TargetLUCNames %in% NonVegetatedLUCs ~ F,
        T ~ T
      ),
      StokesNumber = CalculateStokesNumber(
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        SettlingVelocity_ms = SettlingVelocity_ms,
        CharacteristicRadius_m = CharacteristicRadius_m,
        KinematicViscosityOfAir_m2s = KinematicViscosityOfAir_m2s,
        SurfaceIsVegetated = SurfaceIsVegetated
      ),
      # _E_Im----
      # Loss efficiency by impaction
      E_Im = CalculateLossEfficiencyImpaction(
        StokesNumber = StokesNumber,
        ImpactionParameterAlpha = ImpactionParameterAlpha,
        Parametrization = Parametrization
      ),
      # _E_In----
      # Loss efficiency by interception
      E_In = CalculateLossEfficiencyInterception(
        ParticleDiameter_m = ParticleDiameter_m,
        CharacteristicRadius_m = CharacteristicRadius_m,
        Parametrization = Parametrization
      ),
      # _R_s-----
      # Surface resistance
      R_s_sm = CalculateSurfaceResistance(
        SurfaceIsWet = SurfaceIsWet_bool,
        FrictionVelocity_ms = FrictionVelocityTargetLUC_ms,
        StokesNumber = StokesNumber,
        E_b = E_b,
        E_Im = E_Im,
        E_In = E_In,
        ParticleDiameter_m = ParticleDiameter_m,
        Parametrization = Parametrization
      ),
      # _V_d---------
      V_d_RefHeight_ms = SettlingVelocity_ms + 1 / (R_a_sm + R_s_sm)
    )


  # Sanity checks-----
  Results <- Results %>%
    mutate(
      # Wind speed must be a monotonic function of height
      WindSpeedOK = case_when(
        (AnemometerHeight_m <= WindSpeedBlendingHeight_m) & (WindSpeedAtAnemometerHeight_ms <= WindSpeedAtBlendingHeight_ms) ~ T,
        (AnemometerHeight_m >= WindSpeedBlendingHeight_m) & (WindSpeedAtAnemometerHeight_ms >= WindSpeedAtBlendingHeight_ms) ~ T,
        T ~ F
      )
    )

  if (!all(Results$WindSpeedOK)) {
    stop("Calculated WindSpeedAtBlendingHeight_ms is not a monotonic function of height (by comparison to WindSpeedAtAnemometerHeight_ms).")
  }

  Results <- Results %>%
    select(-WindSpeedOK)



  # Return resuts
  return(Results)
}
