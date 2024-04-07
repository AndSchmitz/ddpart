#' @title CalculateSchmidtNumber
#'
#' @description Calculates the Schmidt number according to Seinfeld and Pandis
#'   (2006) page 574
#'
#' @param DynamicViscosityAir_kgms Dynamic viscosity of air in kg/(m*s). E.g.
#'   provided by CalculateDynamicViscosityOfAir().
#'
#' @param KinematicViscosityOfAir_m2s Kinematic viscosity of air in m2/2. E.g.
#'   provided by CalculateKinematicViscosityOfAir().
#'
#' @param T_air_K Air temperature in Kelvin.
#'
#' @param ParticleDiameter_m Particle diameter in m.
#'
#' @return Schmidt number
#'
#' @examples
#'
#' # Reproduce relation between brownian diffusivity and particle diameter as shown
#' # in Seinfeld and Pandis (2006) page 417 Figure 9.8
#'
#' data(diffusion_validation)
#' PlotData <- data.frame(
#'   # Set standard atmospheric conditions
#'   T_air_K = 20,
#'   AirPressure_Pa = 101325,
#'   # Set particle diameter according to Seinfeld and Pandis (2006)
#'   ParticleDiameter_m = 10^seq(-9, -5, length.out = 50)
#' ) %>%
#'   # Calculation of quantities related to diffusion
#'   mutate(
#'     DynamicViscosityAir_kgms = CalculateDynamicViscosityOfAir(T_air_K),
#'     AirDensity_kgm3 = CalculateAirDensity(
#'       AirPressure_Pa = AirPressure_Pa,
#'       T_air_K = T_air_K
#'     ),
#'     KinematicViscosityOfAir_m2s = CalculateKinematicViscosityOfAir(
#'       DynamicViscosityAir_kgms = DynamicViscosityAir_kgms,
#'       AirDensity_kgm3 = AirDensity_kgm3
#'     ),
#'     SchmidtNumber = CalculateSchmidtNumber(
#'       DynamicViscosityAir_kgms = DynamicViscosityAir_kgms,
#'       KinematicViscosityOfAir_m2s = KinematicViscosityOfAir_m2s,
#'       T_air_K = T_air_K,
#'       ParticleDiameter_m = ParticleDiameter_m
#'     ),
#'     # Calculate brownian diffusivity from intermediate results
#'     BrownianDiffusivity_m2s = KinematicViscosityOfAir_m2s / SchmidtNumber,
#'     # For plotting:
#'     BrownianDiffusivity_cm2s = BrownianDiffusivity_m2s * 1e4,
#'     ParticleDiameter_um = ParticleDiameter_m * 1e6,
#'     Type = "Results from ddpart"
#'   )
#'
#' # Plot
#' if (require("ggplot2")) {
#'   ggplot() +
#'     geom_line(
#'       data = diffusion_validation %>%
#'         mutate(
#'           Type = "Validation data from Seinfeld and Pandis (2006)"
#'         ),
#'       mapping = aes(
#'         x = log10(ParticleDiameter_um),
#'         y = log10(DiffusionCoefficient_cm2s),
#'         color = Type
#'       )
#'     ) +
#'     geom_point(
#'       data = PlotData,
#'       mapping = aes(
#'         x = log10(ParticleDiameter_um),
#'         y = log10(BrownianDiffusivity_cm2s),
#'         color = Type
#'       ),
#'       shape = "x"
#'     ) +
#'     annotate(
#'       geom = "text",
#'       x = -0,
#'       y = -3,
#'       label = "Small differences likely related\nto errors from extraction of
#'     validation data from PDF figure."
#'     ) +
#'     theme(
#'       legend.position = "bottom"
#'     )
#' }
#'
#' @export
#'
#' @references Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From
#'   Air Pollution to Climate Change. Wiley; 2006.
#'
#'
CalculateSchmidtNumber <- function(DynamicViscosityAir_kgms,
                                   KinematicViscosityOfAir_m2s,
                                   T_air_K,
                                   ParticleDiameter_m) {



  # Sanity checks
  InputLength <- length(DynamicViscosityAir_kgms)
  if (
    (length(KinematicViscosityOfAir_m2s) != InputLength) |
      (length(T_air_K) != InputLength) |
      (length(ParticleDiameter_m) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  Dat <- data.frame(
    DynamicViscosityAir_kgms = DynamicViscosityAir_kgms,
    KinematicViscosityOfAir_m2s = KinematicViscosityOfAir_m2s,
    T_air_K = T_air_K,
    ParticleDiameter_m = ParticleDiameter_m
  ) %>%
    mutate(
      Order = 1:n(),
      d_p_um = ParticleDiameter_m * 1e6
    )

  # Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
  # 2006 Table 9.3 p. 407 #Slip correction
  CunninghamCorrectionTable <- tribble(
    ~d_p_um, ~CunninghamCorrection,
    0.001, 216,
    0.002, 108,
    0.005, 43.6,
    0.01, 22.2,
    0.02, 11.4,
    0.05, 4.95,
    0.1, 2.85,
    0.2, 1.865,
    0.5, 1.326,
    1, 1.164,
    2, 1.082,
    5, 1.032,
    10, 1.016,
    20, 1.008,
    50, 1.003,
    100, 1.0016
  )

  if (any(Dat$d_p_um < min(CunninghamCorrectionTable$d_p_um))) {
    stop(paste("No Cunningham correction factor for particles smaller than", min(CunninghamCorrectionTable$d_p_um), "um available."))
  }

  # This is a linear interpolation of the cunningham correction factor for the
  # respective particle diameters
  # First filter those particles exactly matching a row in the
  # CunninghamCorrectionTable
  Dat <- Dat %>%
    mutate(
      IsExactMatch = d_p_um %in% CunninghamCorrectionTable$d_p_um,
      ExceedsCunninghamTable = d_p_um > max(CunninghamCorrectionTable$d_p_um)
    )

  # Add Cunningham correction to exact matches
  ExactMatches <- Dat %>%
    filter(
      IsExactMatch
    ) %>%
    merge(
      y = CunninghamCorrectionTable,
      by = "d_p_um",
      all.x = T
    )

  # Treat very large particles
  VeryLargeParticles <- Dat %>%
    filter(
      ExceedsCunninghamTable
    ) %>%
    mutate(
      CunninghamCorrection = max(CunninghamCorrectionTable$CunninghamCorrection)
    )

  # Treat non matches
  NonMatches <- Dat %>%
    filter(
      !IsExactMatch,
      !ExceedsCunninghamTable
    ) %>%
    mutate(
      # Create x1, x2, y1, y2 for linear interpolation
      d_p_um_Low = NA_real_,
      d_p_um_High = NA_real_,
      CC_Low = NA_real_,
      CC_High = NA_real_
    )
  for (i in 2:nrow(CunninghamCorrectionTable)) {
    d_p_um_Low <- CunninghamCorrectionTable$d_p_um[i - 1]
    d_p_um_High <- CunninghamCorrectionTable$d_p_um[i]
    CC_Low <- CunninghamCorrectionTable$CunninghamCorrection[i - 1]
    CC_High <- CunninghamCorrectionTable$CunninghamCorrection[i]
    # Find the two rows in the CunninghamCorrectionTable between the particle
    # size is located
    idx_BetweenCurrentRows <- which(
      is.na(NonMatches$d_p_um_Low) &
        (NonMatches$d_p_um > d_p_um_Low) &
        (NonMatches$d_p_um < d_p_um_High)
    )
    if (length(idx_BetweenCurrentRows) > 0) {
      NonMatches$d_p_um_Low[idx_BetweenCurrentRows] <- d_p_um_Low
      NonMatches$d_p_um_High[idx_BetweenCurrentRows] <- d_p_um_High
      NonMatches$CC_Low[idx_BetweenCurrentRows] <- CC_Low
      NonMatches$CC_High[idx_BetweenCurrentRows] <- CC_High
    }
  }

  # Sanity check
  if (any(is.na(NonMatches$d_p_um_Low))) {
    stop("Error in linear interpolation of Cunningham correction table")
  }

  # Do the interpolation
  NonMatches <- NonMatches %>%
    mutate(
      delta_d_p_um = d_p_um_High - d_p_um_Low,
      delta_CC = CC_High - CC_Low,
      slope = delta_CC / delta_d_p_um,
      CunninghamCorrection = CC_Low + slope * (d_p_um - d_p_um_Low)
    )

  # Sanity check
  if (any(is.na(NonMatches$CunninghamCorrection))) {
    stop("Error in linear interpolation of Cunningham correction table: CunninghamCorrection")
  }

  # Join all three cases
  DatCC <- bind_rows(ExactMatches, VeryLargeParticles, NonMatches)

  # Continue with standard calculations for Schmidt number
  k <- GetConstants()$k
  DatCC <- DatCC %>%
    mutate(
      # Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
      # 2006 Eq. 9.73 page 416
      BrownianDiffusivity = k * T_air_K * CunninghamCorrection / (3 * pi * DynamicViscosityAir_kgms * ParticleDiameter_m),
      # Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
      # 2006 p. 574
      SchmidtNumber = KinematicViscosityOfAir_m2s / BrownianDiffusivity
    ) %>%
    arrange(Order)

  return(DatCC$SchmidtNumber)
}
