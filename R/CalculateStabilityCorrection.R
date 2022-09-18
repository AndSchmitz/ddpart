# Integrated integrated stability correction for heat
# Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation to Acidification and
# Eutrophication. 1995. Page 67.

CalculateStabilityCorrection <- function(Numerator, # sometimes (z-d), sometimes z0,
                                         MoninObukhovLength_m,
                                         Type # Heat or Momentum
) {


  # Sanity checks
  InputLength <- length(Numerator)
  if (
    (length(MoninObukhovLength_m) != InputLength) |
    (length(Type) != InputLength)
  ) {
    stop("All inputs must have same length.")
  }

  #Define a function that works on one input row at a time.
  CalculateStabilityCorrection_Scalar <- function(
    Numerator, # sometimes (z-d), sometimes z0,
    MoninObukhovLength_m,
    Type # Heat or Momentum
  )  {

    if (!(Type %in% c("Heat", "Momentum"))) {
      stop("(Numerator / MoninObukhovLength_m) \"Type\" must be either \"Heat\" or \"Momentum\".")
    }

    # Monin-Obukhov-length goes to infinity in the limit of perfectly neutral
    # conditions (ED95 p. 66). This is coded by the value specified in
    # GetConstants()$InfLength.
    # In that case, stability correction functions go to 0
    if (MoninObukhovLength_m == GetConstants()$InfLength) {
      return(0)
    }

    # Positive MoninObukhovLength_m indicates stable atmosphere
    # Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
    # 2006. page 750
    AtmosphereIsStable <- ifelse(
      test = MoninObukhovLength_m > 0,
      yes = T,
      no = F
    )

    # Catch division by zero
    if (MoninObukhovLength_m == 0) {
      stop("Cannot calculate stability for MoninObukhovLength_m == 0. ")
    }

    if (AtmosphereIsStable) {
      # psi_h equals psi_m eq. 3.27 page 67
      StabilityCorrection <- -5.2 * (Numerator / MoninObukhovLength_m)
    } else {
      # ED95 eq. 3.28 page 67
      x <- (1 - 16 * (Numerator / MoninObukhovLength_m))^0.25
      if (Type == "Heat") {
        # log() is natural logarithm (ln()) as in ED95.
        StabilityCorrection <- 2 * log((1 + x^2) / 2)
      } else if (Type == "Momentum") {
        # log() is natural logarithm (ln()) as in ED95.
        StabilityCorrection <- 2 * log((1 + x) / 2) + log((1 + x^2) / 2) - 2 * atan(x) + pi / 2
      }
    }

    return(StabilityCorrection)

  } #end of scalar function

  # Vectorize this function
  CalculateStabilityCorrection_Vectorized <- Vectorize(
    FUN = CalculateStabilityCorrection_Scalar,
    USE.NAMES = F
  )

  # Call the vectorized function on the input
  ReturnValue <- CalculateStabilityCorrection_Vectorized(
    Numerator, # sometimes (z-d), sometimes z0,
    MoninObukhovLength_m,
    Type # Heat or Momentum
  )

  return(ReturnValue)

}
