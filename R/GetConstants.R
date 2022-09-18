#' @title GetConstants
#'
#' @description This function returns empirical constants for dry deposition sub-processes (Emerson et al. (2020), Zhang et al. (2020), etc.) and some other basic constants (Von Karman constant, Boltzmann's constant, etc.).
#' @return A named list of parameters. Variables that contain the string "E_20" in their name are taken from Emerson et al. (2020).
#' @examples GetConstants()
#' @export
#' @references
#' Zhang L, He Z. Technical Note: An empirical algorithm estimating dry deposition velocity of fine, coarse and giant particles. Atmospheric Chemistry and Physics 2014;14:3729–3737.
#' Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.
#' Emerson EW, Hodshire AL, DeBolt HM, Bilsback KR, Pierce JR, McMeeking GR, Farmer DK. Revisiting particle dry deposition and its role in radiative effect estimates. Proceedings of the National Academy of Sciences 2020;117:26076–26082.


GetConstants = function(
    Parametrization = "Emerson20"
) {

  #Sanity check
  ValidParametrizations <- c("Emerson20", "Zhang01")
  if ( !(Parametrization %in% ValidParametrizations) ) {
    stop(paste("Parameter Parametrization must be on of", paste(ValidParametrizations, collapse = ",")))
  }

  Constants = list(
    RoundingPrecision = 4,
    g = 9.81,
    #Von Karman constant
    #https://en.wikipedia.org/wiki/Von_K%C3%A1rm%C3%A1n_constant
    kappa = 0.41,
    #Universal gas constant
    R = 8.314, #m3 * Pa / (K * mol)
    #Molar mass of dry air
    #Seinfeld and Pandis eq. 1.2
    M = 28.97/1000, #kg/mol
    #Boltzmann's constant
    k = 1.38 * 1e-23, #J/K
    #Coding for infinite Monin-Obukhov length
    InfLength = -9999,
    #Empirical constant for calculation of R_s from Zhang et al. 2001 eq. 5 also used in
    #Emerson et al. 2020
    epsilon_0 = 3,
    #Empirical constants for calculation of E_b
    C_b = case_when(
      #Emerson et al. 2020 table S1
      Parametrization == "Emerson20" ~ 0.2,
      Parametrization == "Zhang01" ~ 1,
      T ~ NA_real_
    ),
    #Empirical constants for calculation of E_Im from Emerson et al. 2020 table S1
    beta = case_when(
      #Emerson et al. 2020 table S1
      Parametrization == "Emerson20" ~ 1.7,
      Parametrization == "Zhang01" ~ 2,
      T ~ NA_real_
    ),
    C_Im = case_when(
      #Emerson et al. 2020 table S1
      Parametrization == "Emerson20" ~ 0.4,
      Parametrization == "Zhang01" ~ 1,
      T ~ NA_real_
    ),
    #Empirical constants for calculation of E_In from Emerson et al. 2020 table S1
    nu = case_when(
      #Emerson et al. 2020 table S1
      Parametrization == "Emerson20" ~ 0.8,
      Parametrization == "Zhang01" ~ 2,
      T ~ NA_real_
    ),
    C_In = case_when(
      #Emerson et al. 2020 table S1
      Parametrization == "Emerson20" ~ 2.5,
      Parametrization == "Zhang01" ~ 0.5,
      T ~ NA_real_
    )
  )
  return(Constants)
}
