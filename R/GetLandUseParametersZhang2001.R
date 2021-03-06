#' @title GetLandUseParametersZhang2001
#'
#' @description Get land use parameters according to Zhang et al. (2001) table 3. The function is vectorized with respect to parameters "LUC" and "Seasons". I.e. these two parameters must be vectors of same length.
#' @param LUCs A vector of land use class codes (integer values). Currently, only land use classes 1-7 are implemented.
#' @param Seasons A vector of season codes (integer values 1-5).
#' @param TargetPar A string indicating which parameter to return from Zhang et al. (2001) table 3 ("z_0", "A", "alpha" or "gamma").
#' @return A vector of values for parameter "TargetPar".
#' @examples GetLandUseParametersZhang2001(LUCs = c(1,2), Seasons = c(2,5), TargetPar = "A")
#' @export
#' @import dplyr
#' @references
#' Zhang L, Gong S, Padro J, Barrie L. A size-segregated particle dry deposition scheme for an atmospheric aerosol module. Atmospheric Environment 2001;35:549–560.

GetLandUseParametersZhang2001 <- function(
  LUCs,
  Seasons,
  TargetPar
) {

  #From Zhang et al. 2001 table 3
  LUCParsTable <- tribble(
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
    2,"A",5,5,
    2,"alpha",999,0.6,
    2,"gamma",999,0.58,
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
    6,"gamma",999,0.54,
    7,"z_0",1,0.1,
    7,"z_0",2,0.1,
    7,"z_0",3,0.02,
    7,"z_0",4,0.02,
    7,"z_0",5,0.05,
    7,"A",1,2,
    7,"A",2,2,
    7,"A",3,5,
    7,"A",4,5,
    7,"A",5,2,
    7,"alpha",999,1.2,
    7,"gamma",999,0.54
  )
  #Sanity check for number of rows in LUCParsTable
  #7 LUCs, 5 seasons
  # nrow(LUCParsTable) == (7 * (5 * 2 + 2))


  #_Sanity checks of function arguments-----
  LUCsFail <- unique(LUCs[!(LUCs %in% LUCParsTable$LUC)])
  if ( length(LUCsFail) > 0 ) {
    stop(paste("Some LUCs not valid:",paste(LUCsFail, collapse = ",")))
  }
  SeasonsFail <- unique(Seasons[!(Seasons %in% LUCParsTable$Season)])
  if ( length(SeasonsFail) > 0 ) {
    stop(paste("Some Seasons not valid:",paste(SeasonsFail, collapse = ",")))
  }
  if ( !(TargetPar %in% LUCParsTable$Parameter)) {
    stop(paste("Parameter",TargetPar,"is not in LUCParsTable"))
  }

  #Merge target LUCs and Seasons with LUCParsTable to extract
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
    x = LUCParsTable,
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
