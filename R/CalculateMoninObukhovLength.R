#Monin-Obukhov-Length
#Seinfeld JH, Pandis SN. Atmospheric Chemistry and Physics: From Air Pollution to Climate Change.
#2006. Ppage 751.

CalculateMoninObukhovLength <- function(
  PasquillClass,
  z_0
) {
  PasquillObukhovPars <- tribble(
    ~PasquillClass, ~a, ~b,
    "A",-0.096,0.029,
    "B",-0.037,0.029,
    "C",-0.002,0.018,
    "D",0,0,
    "E",0.004,-0.018,
    "F",0.035,-0.036
  )
  
  if ( !(PasquillClass %in% PasquillObukhovPars$PasquillClass) ) {
    stop(paste("Error in function CalculateMoninObukhovLength(): Pasquill class",PasquillClass,"not implemented."))
  }
  
  idx <- which(PasquillObukhovPars$PasquillClass == PasquillClass)
  a <- PasquillObukhovPars$a[idx]
  b <- PasquillObukhovPars$b[idx]
  if ( (a==0) & (b==0) ) {
    #Monin-Obukhov-length goes to infinity in the limit of perfectly neutral
    #conditions. See for example Erisman JW, Draaijers GPJ. Atmospheric Deposition In Relation
    #to Acidification and Eutrophication. 1995. Page 66.
    #This case is coded as GetConstants()$InfLength (-9999)
    return(GetConstants()$InfLength)
  } else {
    #SP06 eq. 16.83
    #log() in SP06 means log10 as reconstructed from example calculation given
    #immediately below SP06 eq. 16.83 (page 751)
    L <- 1 / (a + b * log10(z_0))
    L <- round(L,GetConstants()$RoundingPrecision)
    return(L) 
  }
}
