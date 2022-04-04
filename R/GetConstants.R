GetConstants = function() {
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
    #Empirical constants for calculation of E_b from Emerson et al. 2020 table S1
    C_b_E20 = 0.2,
    Gamma_E20 = 2/3,
    #Empirical constants for calculation of E_Im from Emerson et al. 2020 table S1
    beta_E20 = 1.7,
    C_Im_E20 = 0.4,
    #Empirical constants for calculation of E_In from Emerson et al. 2020 table S1
    nu_E20 = 0.8,
    C_In_E20 = 2.5,
    #Particle size parametrization according to
    #Zhang L, He Z. Technical Note: An empirical algorithm estimating dry deposition velocity of
    #fine, coarse and giant particles. Atmospheric Chemistry and Physics 2014;14:3729–3737.
    GeometricMassMedianDiameterPMcoarse_m = 4.5 * 1e-6, #PM2.5−10
    GeometricStandardDeviationPMcoarse_m = 1.6 * 1e-6, #PM2.5−10
    GeometricMassMedianDiameterPMfine_m = 2.2 * 1e-6, #PM2.5
    GeometricStandardDeviationPMfine_m = 0.4 * 1e-6, #PM2.5
    GeometricMassMedianDiameterPMgiant_m = 20 * 1e-6, #PM10+
    GeometricStandardDeviationPMgiant_m = 1.6 * 1e-6 #PM10+
  )
  return(Constants)
}
