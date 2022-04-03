#Land use specific parameters
#Emerson et al. 2020
#and
#Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an
#atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.
#able 3
GetLandUseParametersEmerson2020 <- function(
  LUCs = -9,
  Seasons = -9,
  TargetPar = "undef"
) {

  #This function is vectorized
  
  #Table 3
  #FIXME check vlaues again
  ConstantTable <- tribble(
    ~LUC,~Parameter,~Season,~Value,
    1,"z_0",1,0.8,
    1,"z_0",2,0.9,
    1,"z_0",3,0.9,
    1,"z_0",4,0.9,
    1,"z_0",5,0.8,
    1,"A",1,2,
    1,"A",2,2,
    1,"A",3,2,
    1,"A",4,2,
    1,"A",5,2,
    1,"alpha",999,1,
    1,"gamma",999,0.56,
    2,"z_0",1,2.65,
    2,"z_0",2,2.65,
    2,"z_0",3,2.65,
    2,"z_0",4,2.65,
    2,"z_0",5,2.65,
    2,"A",1,5,
    2,"A",2,5,
    2,"A",3,5,
    2,"A",4,5,
    2,"alpha",999,0.6,
    2,"gamma",999,0.58,
    3,"A",5,5,
    3,"z_0",1,0.85,
    3,"z_0",2,0.85,
    3,"z_0",3,0.8,
    3,"z_0",4,0.55,
    3,"z_0",5,0.6,
    3,"A",1,2,
    3,"A",2,2,
    3,"A",3,5,
    3,"A",4,5,
    3,"A",5,2,
    3,"alpha",999,1.1,
    3,"gamma",999,0.56,
    4,"z_0",1,1.05,
    4,"z_0",2,1.05,
    4,"z_0",3,0.95,
    4,"z_0",4,0.55,
    4,"z_0",5,0.75,
    4,"A",1,5,
    4,"A",2,5,
    4,"A",3,10,
    4,"A",4,10,
    4,"A",5,5,
    4,"alpha",999,0.8,
    4,"gamma",999,0.56,
    5,"z_0",1,1.15,
    5,"z_0",2,1.15,
    5,"z_0",3,1.15,
    5,"z_0",4,1.15,
    5,"z_0",5,1.15,
    5,"A",1,5,
    5,"A",2,5,
    5,"A",3,5,
    5,"A",4,5,
    5,"A",5,5,
    5,"alpha",999,0.8,
    5,"gamma",999,0.56,
    6,"z_0",1,0.1,
    6,"z_0",2,0.1,
    6,"z_0",3,0.05,
    6,"z_0",4,0.02,
    6,"z_0",5,0.05,
    6,"A",1,2,
    6,"A",2,2,
    6,"A",3,5,
    6,"A",4,5,
    6,"A",5,2,
    6,"alpha",999,1.2,
    65,"gamma",999,0.54    
  )

  # ValidLUCs <- c(
  #   1, #Evergreen needleleaf trees
  #   2, #Evergreen broadleaf trees
  #   3, #Deciduous needleleaf trees
  #   4, #Deciduous broadleaf trees
  #   5, #Mixed broadleleaf and needleaf trees
  #   6  #Grass
  # )
  # ValidSeasons <- c(
  #   1, #Midsummer with lush vegetation
  #   2, #Autumn with cropland that has not been harvested
  #   3, #Late autumn after frost, no snow.
  #   4, #Winter, snow on ground and sub-freezing.
  #   5  #Transitional spring with partially green short annuals
  # )
  
  
  #_Sanity checks of function arguments-----
  LUCsFail <- unique(LUCs[!(LUCs %in% ConstantTable$LUC)])
  if ( length(LUCsFail) > 0 ) {
    stop(paste("Some LUCs not valid:",paste(LUCsFail, collapse = ",")))
  }
  SeasonsFail <- unique(Seasons[!(Seasons %in% ConstantTable$Season)])
  if ( length(SeasonsFail) > 0 ) {
    stop(paste("Some Seasons not valid:",paste(SeasonsFail, collapse = ",")))
  }
  if ( !(TargetPar %in% ConstantTable$Parameter)) {
    stop(paste("Parameter",TargetPar,"is not in ConstantTable"))
  }
  
  #Merge target LUCs and Seasons with ConstantTable to extract
  #relevant information. Parameters z_0 and A are dependent on season,
  #parameters alpha and gamma only depend on LUC
  MergeBy = case_when(
    TargetPar %in% c("z_0","A") ~ c("LUC","Season"),
    TargetPar %in% c("alpha","gamma") ~ c("LUC")
  )
  #merge() changes the order of rows! Define a column "Order" to 
  #reconstruct the original order.
  FilterDF <- data.frame(
    LUC = LUCs,
    Season = Seasons
  ) %>%
    mutate(
      Order = 1:n()
    )
  SelectedPars <- merge(
    x = ConstantTable,
    y = FilterDF,
    by = MergeBy,
    all.y = T
  ) %>%
    filter(
      Parameter == TargetPar
    ) %>%
    arrange(Order) %>%
    select(-Order)
  
  if ( nrow(SelectedPars) == 0 ) {
    stop("nrow(SelectedPars) == 0")
  }

  return(SelectedPars$Value)
  
}