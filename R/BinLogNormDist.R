BinLogNormDist <- function(
  MassMedianDiameter_um,
  GeometricStandardDeviation_um
) {
  #Function to calculate the share of total air concentration that falls within
  #each bin for log normal distribution characterzized by MassMedianDiameter_um and
  #GeometricStandardDeviation_um. The distribution is split into nBins which are
  #distributed linearly (equal width) if BinsLogOrLin="lin" or logarithmically
  #(ln, not log10) if BinsLogOrLin="log"
  #
  #Calculations
  # - BinDiamLow_um and BinDiamHigh_um: Upper and lower diameter limits (um) define
  #the x-axis position of the i'th bin.
  # - ConcShare: This is the share of the overall air concentration for the i'th
  #bin. The sum of ConcShare over all bins is one. ConcShare is calculated as
  #CDF(BinDiamHigh_um) - CDF(BinDiamLow_um), where CDF is the cumulative
  #distribution function of the log normal distribution.
  #
  #mu and sigma according to interpretation from here:
  #https://en.wikipedia.org/wiki/Log-normal_distribution
  mu = log(MassMedianDiameter_um)
  sigma = log(GeometricStandardDeviation_um)
  #
  #
  #Define how to create the bins
  #
  #
  #Option 1
  #Lower bin border of the first bin is a very low quantile of the log normal
  #distribution (LND). Higher bin border of the last bin is a very high quantile
  #of the log normal distribution (LND).
  Q01_um_lin = qlnorm(p = 0.0001, meanlog = mu, sdlog = sigma)
  Q99_um_lin = qlnorm(p = 0.9999, meanlog = mu, sdlog = sigma)
  #
  #Option 1A: Linear scaling of bin borders
  #BinBorders <- seq(Q01_um_lin, Q99_um_lin, length.out = nBins+1)
  #
  #Option 1B: Natural log scaling of bin borders
  #BinBorders <- exp(seq(log(Q01_um_lin), log(Q99_um_lin), length.out = nBins+1))
  #
  #Option 2:
  #Custom settings:
  # - bin borders exactly matching 1, 2.5 am 10 um
  # - bin borders independent of MMD and GSD
  # - bin borders with log scaling in between
  #Binning starts at the particle diameter that matches the first percentile of the ZH14
  #size parametrization for PMfine.
  LowestBinBorder <- qlnorm(p = 0.01, meanlog = log(0.4), sdlog = log(2.2))
  #Binning emd at the particle diameter that matches the last percentile of the ZH14
  #size parametrization for PMcoarse.  
  HighestBinBorder <- qlnorm(p = 0.99, meanlog = log(4.5), sdlog = log(1.6))
  #The following numbers define the number of bins in the following classes:
  # - <1um
  # - between 1 and 2.5 um
  # - between >2.5um
  # - >10um
  nBinsVeryFine <- 10
  nBinsFine <- 5
  nBinsCoarse <- 10
  nBinsGiant <- 5
  #Generate natural-log-scaled bin borders
  BinBordersVeryFine <- exp(seq(log(LowestBinBorder), log(1), length.out = nBinsVeryFine+1))
  BinBordersFine <- exp(seq(log(1), log(2.5), length.out = nBinsFine+1))
  BinBordersCoarse <- exp(seq(log(2.5), log(10), length.out = nBinsCoarse+1))
  BinBordersGiant <- exp(seq(log(10), log(HighestBinBorder), length.out = nBinsGiant+1))
  BinBorders <- unique(c(BinBordersVeryFine, BinBordersFine, BinBordersCoarse,BinBordersGiant))
  #
  #
  #Calculations independent on bin calculation variant
  #
  #
  nBins <- length(BinBorders) -1
  Bins <- data.frame(
    BinID = 1:nBins,
    #upper and lower diameter limits (um)
    BinDiamLow_um = BinBorders[1:nBins],
    BinDiamHigh_um = BinBorders[2:(nBins+1)]
  ) %>%
    mutate(
      #Values of cumulative distribution function at lower and higher
      #bin borders
      CDFBin_low = stats::plnorm(q = BinDiamLow_um, meanlog = mu, sdlog = sigma),
      CDFBin_high = stats::plnorm(q = BinDiamHigh_um, meanlog = mu, sdlog = sigma),
      #Concentration share for the respective bin
      ConcShare = CDFBin_high - CDFBin_low,
      #sum(ConcShare) is not perfectly equal to 1, but because min(BinDiamLow_um) might not be
      #zero and max(BinDiamHigh_um) is not infinity. Thus, manually rescale concentration shares
      #to 1, to avoid systematic errors.
      ConcShare = ConcShare / sum(ConcShare),
    )
  if ( sum(Bins$ConcShare) != 1 ) {
    stop("sum(Bins$ConcShare) != 1")
  }
  return(Bins)
}