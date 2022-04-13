CalculateHygroscopicSwelling <- function(
  DryParticleDiameter_m,
  AerosolType,
  RelHum_percent
) {

  #This function is vectorized with respect to all parameters

  #Zhang et al. 2001 Table 1:
  HygroscopicSwellingPars <- tribble(
    ~AerosolType, ~C1, ~C2, ~C3, ~C4,
    "SeaSalt", 0.7674, 3.079, 2.573*1e-11, -1.424,
    "Urban", 0.3926, 3.101, 4.190*1e-11, -1.404,
    "Rural", 0.2789, 3.115, 5.415*1e-11, -1.399,
    "AmmoniumSulfate", 0.4809, 3.082, 3.110*1e-11, -1.428,
    #extra row to disable hygroscopic swelling
    "Dry", -9999, -9999, -9999, -9999
  )

  if ( !(all(AerosolType %in% c("Dry", HygroscopicSwellingPars$AerosolType))) ) {
    stop(paste("All values for parameter AerosolType must be in ", paste(unique(HygroscopicSwellingPars$AerosolType, collapse = ","))))
  }

  Data <- data.frame(
    DryRadius_m = DryParticleDiameter_m / 2,
    AerosolType = AerosolType,
    RelHum_percent = RelHum_percent
  ) %>%
    merge(
      y = HygroscopicSwellingPars,
      by = "AerosolType"
    ) %>%
    mutate(
      #FIXME: BOTH aspects need checking / communication
      #wet particle radius
      #Comment 1: It is assumed that "log" in Zhang et al. 2001 and in the original publication (Gerber 1985)
      #stands for "log10" because a reference to powers of 10 seems to prevail in the
      #original publication (Gerber 1985).
      #Comment 2: While eq. 10 in Zhang et al. 2001 includes the brackets around the whole
      #equation, it misses the power of (1/3) at the end, which is present in the original
      #publication (Gerber 1985 eq. 15 and 22). Thus, ^(1/3) has been added.
      WetRadius_m = case_when(
        #No change if AerosolType == "Dry"
        AerosolType == "Dry" ~ DryRadius_m,
        #Calculation according to Zhang eq 10 otherwiese
        T ~ ((C1 * DryRadius_m^C2) / (C3 * DryRadius_m^C4 - log10(RelHum_percent/100)) + DryRadius_m^3)^(1/3)
      ),
      WetDiameter_m = WetRadius_m * 2
    )

  return(Data$WetDiameter_m)
}
