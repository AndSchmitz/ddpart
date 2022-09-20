#' @title CalculateSchmidtNumber
#'
#' @description Calculates the Schmidt number according to Seinfeld and
#' Pandis (2006) page 574
#'
#' @param DynamicViscosityAir_kgms Dynamic viscosity of air in kg/(m*s). E.g. provided by CalculateDynamicViscosityOfAir().
#'
#' @param KinematicViscosityOfAir_m2s Kinematic viscosity of air in m2/2. E.g. provided by CalculateKinematicViscosityOfAir().
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
#' library(ggplot2)
#' data(diffusion_validation)
#' PlotData <- data.frame(
#'   #Set standard atmospheric conditions
#'   T_air_K = 20,
#'   AirPressure_Pa = 101325,
#'   #Set particle diameter according to Seinfeld and Pandis (2006)
#'   ParticleDiameter_m = 10^seq(-9, -5, length.out = 50)
#' ) %>%
#'   #Calculation of quantities related to diffusion
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
#' #Plot
#' ggplot()  +
#'   geom_line(
#'     data = diffusion_validation %>%
#'       mutate(
#'         Type = "Validation data from Seinfeld and Pandis (2006)"
#'       ),
#'     mapping = aes(
#'       x = log10(ParticleDiameter_um),
#'       y = log10(DiffusionCoefficient_cm2s),
#'       color = Type
#'     )
#'   ) +
#'   geom_point(
#'     data = PlotData,
#'     mapping = aes(
#'       x = log10(ParticleDiameter_um),
#'       y = log10(BrownianDiffusivity_cm2s),
#'       color = Type
#'     ),
#'     shape = "x"
#'   ) +
#'   annotate(
#'     geom = "text",
#'     x = -0,
#'     y = -3,
#'     label = "Small differences likely related\nto errors from extraction of
#'     validation data from PDF figure."
#'   ) +
#'   theme(
#'     legend.position = "bottom"
#'   )
#'
#' @export
#'
#' @references Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change. Wiley; 2006.
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


  CalculateSchmidtNumber_Scalar <- function(DynamicViscosityAir_kgms,
                                            KinematicViscosityOfAir_m2s,
                                            T_air_K,
                                            ParticleDiameter_m) {
    d_p_um <- ParticleDiameter_m * 1e6
    if (d_p_um < min(CunninghamCorrectionTable$d_p_um)) {
      stop(paste("No Cunningham correction factor for particles smaller than", min(CunninghamCorrectionTable$d_p_um), "um provided."))
    }
    # Assign Cunningham correction factor
    if (d_p_um > max(CunninghamCorrectionTable$d_p_um)) {
      CunninghamCorrection <- CunninghamCorrectionTable$CunninghamCorrection[CunninghamCorrectionTable$d_p_um == max(CunninghamCorrectionTable$d_p_um)]
    } else {
      idx_d_p <- which(CunninghamCorrectionTable$d_p_um == d_p_um)
      if (length(idx_d_p) > 0) {
        # d_p is exactly matched in table
        CunninghamCorrection <- CunninghamCorrectionTable$CunninghamCorrection[idx_d_p]
      } else {
        # Linearly interpolate Cunningham correction factor for particle size d_p_um
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
    }

    # Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
    # 2006 Eq. 9.73 page 416
    k <- GetConstants()$k
    # FEATURE: Implement test - this results must match Seinfeld and Pandis Fig. 9.8 page 417
    BrownianDiffusivity <- k * T_air_K * CunninghamCorrection / (3 * pi * DynamicViscosityAir_kgms * ParticleDiameter_m)

    # Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
    # 2006 p. 574
    SchmidtNumber <- KinematicViscosityOfAir_m2s / BrownianDiffusivity
    return(SchmidtNumber)
  } # end of scalar function

  # Vectorize this function
  CalculateSchmidtNumber_Vectorized <- Vectorize(
    FUN = CalculateSchmidtNumber_Scalar,
    USE.NAMES = F
  )

  # Call the vectorized function on the input
  ReturnValue <- CalculateSchmidtNumber_Vectorized(
    DynamicViscosityAir_kgms,
    KinematicViscosityOfAir_m2s,
    T_air_K,
    ParticleDiameter_m
  )

  return(ReturnValue)
}
