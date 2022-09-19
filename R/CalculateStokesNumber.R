#' @title CalculateStokesNumber
#'
#' @description Calculates the stokes number according to Zhang et al. (2001)
#'
#' @param FrictionVelocity_ms Friction velocity in m/s
#'
#' @param SettlingVelocity_ms Settling velocity in m/s
#'
#' @param CharacteristicRadius_m Characteristic radius of receptor surface in m
#'
#' @param KinematicViscosityOfAir_m2s Kinematic viscosity of air in m2/s
#'
#' @param SurfaceIsVegetated Boolean value indicating whether the receptor
#' surface is a vegetation surface
#'
#' @return Stokes number
#'
#' @export
#'
#' @references Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle
#' dry deposition scheme for an atmospheric aerosol module.
#' Atmospheric Environment 2001;35:549–560.


CalculateStokesNumber <- function(FrictionVelocity_ms,
                                  SettlingVelocity_ms,
                                  CharacteristicRadius_m,
                                  KinematicViscosityOfAir_m2s,
                                  SurfaceIsVegetated) {

  # Sanity checks
  InputLength <- length(FrictionVelocity_ms)
  if (
    (length(SettlingVelocity_ms) != InputLength) |
      (length(CharacteristicRadius_m) != InputLength) |
      (length(KinematicViscosityOfAir_m2s) != InputLength) |
      (length(SurfaceIsVegetated) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  if (any(is.na(SurfaceIsVegetated))) {
    stop("Could not determine whether surface is vegetated.")
  }

  g <- GetConstants()$g

  ReturnValue <- data.frame(
    SettlingVelocity_ms,
    FrictionVelocity_ms,
    CharacteristicRadius_m,
    KinematicViscosityOfAir_m2s,
    SurfaceIsVegetated
  ) %>%
    mutate(
      StokesNumber = case_when(
        SurfaceIsVegetated ~ SettlingVelocity_ms * FrictionVelocity_ms / (g * CharacteristicRadius_m),
        !SurfaceIsVegetated ~ SettlingVelocity_ms * FrictionVelocity_ms^2 / KinematicViscosityOfAir_m2s
      )
    )

  return(ReturnValue$StokesNumber)
}
