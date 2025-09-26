#' @title CalculateDepositionVelocity
#'
#' @description Calculates the deposition velocity of particle according to the
#'   Zhang et al. (2001) and the Emerson et al. (2020) publications.
#'
#'   This is a wrapper function over the various dry deposition sub-processes.
#'   It is designed for use cases where information on wind speed is
#'   available for the same land use type where deposition velocities should be
#'   calculated (e.g. wind speed measured at 10 m anemometer height and
#'   concentrations at 1.5 m, both over grassland.) Use
#'   CalculateDepositionVelocity2() for situations where wind speed data is
#'   available for one land use but dry deposition velocities should be
#'   calculated for another land use.
#'
#'   Required inputs are:
#'
#'   (1) basic meteorological data (wind speed, global ration, relative
#'   humidity, cloud cover, air temperature, air pressure, surface wetness)
#'
#'   (2) information on the surface properties (land use class, roughness length
#'   and displacement height) and reference height (height of concentration
#'   measurements)
#'
#'   (3) particle properties (dry particle diameter, density, optional: aerosol
#'   type to calculate hygroscopic swelling)
#'
#'   The calculation steps in CalculateDepositionVelocity() are:
#'
#'   1. Calculate meteorological basics (e.g. viscosity and density of air) and
#'   whether its day of night (based on parameter SunAngle_degree).
#'
#'   2. Calculate the Pasquill class (general classification of atmospheric
#'   stability).
#'
#'   3. Calculate other meteorological parameters (Monin-Obukhov length,
#'   friction velocity, stability corrections).
#'
#'   4. Calculate the aerodynamic resistance (Ra) between the reference height
#'   and the effective height of the receptor (displacement height plus
#'   roughness length). Calculate surface resistance (Rs) and
#'   gravitation settling.
#'
#'   5. Calculate dry deposition velocity.
#'
#' @param InputTable A data frame with the following columns:
#'
#'   - SunAngle_degree: The sun angle in degree. Only used to determine if its
#'   "day" (> 0°) or "night" (< 0°) which is required for the determination of
#'   the Pasquill stability class (GetPasquillClass()). The sun angle can easily
#'   be calculated with oce::sunAngle().
#'
#'   - T_air_K: Air temperature in Kelvin.
#'
#'   - AirPressure_Pa: Air pressure in Pa. Required for the calculation of the
#'   mean free path of an air molecule, which is required for the calculation of
#'   the particle settling (sedimentation) velocity.
#'
#'   - GlobalRadiation_W_m2: Global radiation in W/m2. Required for the
#'   determination of the Pasquill stability class (GetPasquillClass()).
#'
#'   - CloudCover_percent: Cloud cover in percent. Required for the
#'   determination of the Pasquill stability class (GetPasquillClass()).
#'
#'   - WindSpeedAtAnemometerHeight_ms: Wind speed (m/s) at the the anemometer
#'   height. Required for GetPasquillClass() and CalculateFrictionVelocity().
#'
#'   - RoughnessLength_m: Roughness length (m) of the land cover for which the
#'   anemometer wind speed is provided. Required for multiple functions.
#'
#'   - ZeroPlaneDisplacementHeight_m: Zero plane displacement height (m) of the
#'   land cover for which the anemometer wind speed is provided. Required for
#'   multiple functions.
#'
#'   - AnemometerHeight_m: Height to which the wind speed data refers
#'
#'   - SurfaceIsWet_bool: Boolean indicating whether the surface is wet, in
#'   which case the particle rebound effect is disabled. Can be set e.g.
#'   depending on relative humidity and/or precipitation events.
#'
#'   - SurfaceIsVegetated_bool: Boolean indicating whether the surface is
#'   vegetated. Should be TRUE for all LUCs currently implemented in ddpart.
#'
#'   - LUCNames: Land use classes (character). See ?GetLandUseParameters for
#'   information.
#'
#'   - ReferenceHeight_m: Aeordynamic resistance is calculated between
#'   ReferenceHeight_m and the effective receptor height (RoughnessLength_m +
#'   ZeroPlaneDisplacementHeight_m).
#'
#'   - RoughnessLength_m: Roughness length of the land use class.
#'
#'   - ZeroPlaneDisplacementHeight_m: Zero-plane displacement height of the land
#'    use class.
#'
#'   - Season: Integer determining the season according to Zhang
#'   et al. (2001) table 2.
#'
#'   - DryParticleDiameter_m: Particle diameter in m before the growth particles
#'   due to water uptake is taken into account (see parameter "AerosolType").
#'
#'   - ParticleDensity_kgm3: Density of the particles in kg/m3.
#'
#'   - AerosolType: ´Set parameter AerosolType to "Dry" to disable calculation of
#'   hygroscopic swelling. Set parameter "AerosolType" to one of "SeaSalt",
#'   "Urban", "Rural", "AmmoniumSulfate" to enable hygroscopic swelling
#'   according to Zhang et al. (2001). See CalculateHygroscopicSwelling() for
#'   further details.
#'
#'   - RelHum_percent: Relativ humidty (%) for hygroscopic swelling of
#'   particles.
#'
#'   - Parametrization: A character indicates which parametrization should be
#'   used. See ?GetLandUseParameters for allowed values.
#'
#' @return A data frame repeating the InputTable plus additional columns with
#'   calculated values.
#'
#' @examples # See vignette.
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


CalculateDepositionVelocity <- function(InputTable) {

  # Sanity-check for required column names
  RequiredColumns <- c(
    # Meteo
    "SunAngle_degree", "T_air_K", "AirPressure_Pa", "GlobalRadiation_W_m2",
    "RelHum_percent", "CloudCover_percent", "WindSpeedAtAnemometerHeight_ms",
    "AnemometerHeight_m",  "SurfaceIsWet_bool", "SurfaceIsVegetated_bool",
    # Receptor properties
    "LUCNames", "ReferenceHeight_m",  "RoughnessLength_m",
    "ZeroPlaneDisplacementHeight_m", "Season",
    # Particle properties
    "DryParticleDiameter_m", "ParticleDensity_kgm3", "AerosolType",
    "Parametrization"
  )
  MissCols <- RequiredColumns[!(RequiredColumns %in% colnames(InputTable))]
  if (length(MissCols) > 0) {
    stop(paste("The following columns are missing in InputTable:", paste(MissCols, collapse = ",")))
  }

  # _Update particle radius by hygroscopic swelling-----
  Results <- InputTable %>%
    mutate(
      ParticleDiameter_m = CalculateHygroscopicSwelling(
        DryParticleDiameter_m = DryParticleDiameter_m,
        AerosolType = AerosolType,
        RelHum_percent = RelHum_percent
      )
    )

  # _Meteorological basics----
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
      ),
    # Atmospheric stability following
    # Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification and
    # Eutrophication. 1995. Page 67.
    # and
    # Pasquill stability class
    # Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
    # 2006
      PasquillClass = GetPasquillClass(
        SurfaceWindSpeed_ms = WindSpeedAtAnemometerHeight_ms,
        DayOrNight = DayOrNight,
        IncomingSolarRadiation_Wm2 = GlobalRadiation_W_m2,
        CloudCover_percent = CloudCover_percent
      ),
      # _Friction/stability parameters for WMO grassland------
      ObukhovLength_Anemometer_m = CalculateMoninObukhovLength(
        PasquillClass = PasquillClass,
        RoughnessLength_m = RoughnessLength_m
      ),
      FrictionVelocity_Anemometer_ms = CalculateFrictionVelocity(
        WindSpeed_ms = WindSpeedAtAnemometerHeight_ms,
        AnemometerHeight_m = AnemometerHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeight_m,
        RoughnessLength_m = RoughnessLength_m,
        MoninObukhovLength_m = ObukhovLength_Anemometer_m
      ),
      CharacteristicRadius_m = 0.001 * GetLandUseParameters(
        LUCNames = LUCNames,
        Seasons = Season,
        TargetParameter = "A_mm",
        Parametrizations = Parametrization
      ),
      ImpactionParameterAlpha = GetLandUseParameters(
        LUCNames = LUCNames,
        Seasons = Season,
        TargetParameter = "alpha",
        Parametrizations = Parametrization
      ),
      # _Aerodynamic resistance-----
      R_a_sm = CalculateAerodynamicResistance(
        FrictionVelocity_ms = FrictionVelocity_Anemometer_ms,
        ReferenceHeight_m = ReferenceHeight_m,
        ZeroPlaneDisplacementHeight_m = ZeroPlaneDisplacementHeight_m,
        RoughnessLength_m = RoughnessLength_m,
        MoninObukhovLength_m = ObukhovLength_Anemometer_m
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
        LUCNames = LUCNames,
        Seasons = Season,
        TargetParameter = "gamma",
        Parametrizations = Parametrization
      ),
      E_b = CalculateLossEfficiencyBrownianDiffusion(
        SchmidtNumber = SchmidtNumber,
        BrownianDiffusionParameterGamma = BrownianDiffusionParameterGamma,
        Parametrization = Parametrization
      ),
      # _Stokes number-----
      StokesNumber = CalculateStokesNumber(
        FrictionVelocity_ms = FrictionVelocity_Anemometer_ms,
        SettlingVelocity_ms = SettlingVelocity_ms,
        CharacteristicRadius_m = CharacteristicRadius_m,
        KinematicViscosityOfAir_m2s = KinematicViscosityOfAir_m2s,
        SurfaceIsVegetated = SurfaceIsVegetated_bool
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
        Parametrization = Parametrization,
        SurfaceIsVegetated = SurfaceIsVegetated_bool
      ),
      # _R_s-----
      # Surface resistance
      R_s_sm = CalculateSurfaceResistance(
        SurfaceIsWet = SurfaceIsWet_bool,
        FrictionVelocity_ms = FrictionVelocity_Anemometer_ms,
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

  # Return resuts
  return(Results)
}
