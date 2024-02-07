library(maptools)
library(aecisp)
setwd("/media/Hitachi2GB/00NMML/GlacierPhotoSampling/aecisp/aecisp/inst/data/Sim4")
require(splancs, quietly = TRUE)
require(spatstat, quietly = TRUE)


#-------------------------------------------------------------------------------
#  SAMPLE PLOTS
#-------------------------------------------------------------------------------

ni <- 26
nj <- 26
PlotSize <- 0.14
offst <- (10-.14*25)/(2*26)
ep <- .01
i <- 1
j <- 1
samples <- NULL
for(i in 1:ni) {
  for (j in 1:nj) {
    if (i != 3 & j != 2 & j != 4)
    {
        xL <- (i-1)*10/ni + offst
        xU <- (i-1)*10/ni + offst + PlotSize
        yL <- (j-1)*10/nj + offst
        yU <- (j-1)*10/nj + offst + PlotSize
        samples <- c(samples, list( list( x = c(xL, xU, xU, xL),
                                          y = c(yL, yL, yU, yU)
                                        )
                                   )
                     )
    }
  }
}
wsamples <- owin(poly = samples)
#plot(wsamples)

lower.x.lim <- 0
upper.x.lim <- 10
lower.y.lim <- 0
upper.y.lim <- 10
w <- owin(poly = list(
      x = c(lower.x.lim, upper.x.lim, upper.x.lim, lower.x.lim),
      y = c(lower.y.lim, lower.y.lim, upper.y.lim, upper.y.lim)
    )
)
#plot(w, add = TRUE)

#-------------------------------------------------------------------------------
#  SIMULATION
#-------------------------------------------------------------------------------

StoreSims <- NULL

niter <- 500
Start.Time <- Sys.time()
 for (iter in 1:niter) {
  lower.x.bbox <- runif(1, 5.8, 6.2)
  upper.x.bbox <- runif(1, 7.8, 8.2)
  lower.y.bbox <- runif(1, 5.8, 6.2)
  upper.y.bbox <- runif(1, 7.8, 8.2)
  nseed.big <- 75
  nseed.sma <- 15
  Poi.mean.big <- 14
  Poi.mean.sma <- 8
  big.range <- 1
  sma.range <- 0.02
  trend <- TRUE

  PlotSize <- .5
  pcover <- .5
  SampBal <- TRUE

#debug(point.sim.clus)
  Sim <- pointSimClus(nseed.big = nseed.big,
	  nseed.sma = nseed.sma,
	  Poi.mean.big = Poi.mean.big,
	  Poi.mean.sma = Poi.mean.sma,
	  big.range = big.range,
    sma.range = sma.range,
	  lower.x.lim = 0, upper.x.lim = 10,
	  lower.y.lim = 0, upper.y.lim = 10,
	  lower.x.bbox = lower.x.bbox, upper.x.bbox = upper.x.bbox,
	  lower.y.bbox = lower.y.bbox, upper.y.bbox = upper.y.bbox,
    trend = trend)
  SimPts1 <- Sim$SimPts

  lower.x.bbox <- runif(1, 0.8, 1.2)
  upper.x.bbox <- runif(1, 3.8, 4.2)
  lower.y.bbox <- runif(1, 4.8, 5.2)
  upper.y.bbox <- runif(1, 7.8, 8.2)
  nseed.big <- 25
  nseed.sma <- 10
  Poi.mean.big <- 14
  Poi.mean.sma <- 8
  big.range <- .5
  sma.range <- 0.02
  trend <- TRUE

  PlotSize <- .5
  pcover <- .5
  SampBal <- TRUE

  Sim <- pointSimClus(nseed.big = nseed.big,
	  nseed.sma = nseed.sma,
	  Poi.mean.big = Poi.mean.big,
	  Poi.mean.sma = Poi.mean.sma,
	  big.range = big.range,
    sma.range = sma.range,
	  lower.x.lim = 0, upper.x.lim = 10,
	  lower.y.lim = 0, upper.y.lim = 10,
	  lower.x.bbox = lower.x.bbox, upper.x.bbox = upper.x.bbox,
	  lower.y.bbox = lower.y.bbox, upper.y.bbox = upper.y.bbox,
    trend = trend)
  SimPts2 <- Sim$SimPts

  SimPts <- rbind(SimPts1, SimPts2)

  TrueAbundance <- length(SimPts[,1])
  TrueAbundance
  SimPts <- as.points(SimPts[,1], SimPts[,2])

  SimPtsPPP <- ppp(SimPts[,1], SimPts[,2], window = w)

  SimPtsSampled <- SimPtsPPP[wsamples]
#  plot(wsamples)
#  plot(w, add = TRUE)
#  points(SimPts, pch = 19, cex = .5, col = "grey50")
#  plot(SimPtsSampled, pch = 19, cex = .5, add = T)


  wnotsampled <- w
  cumlist <- list(wnotsampled$bdry[[1]])
  cumarea <- 0
  for(i in 1:length(wsamples$bdry)) {
    tmplist <- wsamples$bdry[[i]]
    tmplist$hole <- TRUE
    cumarea <- cumarea + tmplist$area
    cumlist <- c(cumlist,list(tmplist))
  }
  wnotsampled$bdry <- cumlist
  ProportionUnsurveyed <- (100 - cumarea)/100

  SimPtsNotSampled <- SimPtsPPP[wnotsampled]

# ------------------------------------------------------------------------------
# standardize x and y coordinates
# ------------------------------------------------------------------------------

  x <- w$bdry[[1]]$x
  y <- w$bdry[[1]]$y
  noutline <- length(x)
  xmean.fix <- mean(x)
  ymean.fix <- mean(y)
  xystdv.fix <- mean(sqrt(var(x)), sqrt(var(y)))
  SurvArea <- owin(poly = list(
      x = (x - xmean.fix)/xystdv.fix,
      y = (y - ymean.fix)/xystdv.fix
    )
  )
#  plot(SurvArea)

  x <- (SimPts[,1] - xmean.fix)/xystdv.fix
  y <- (SimPts[,2] - ymean.fix)/xystdv.fix
  pointdata <- ppp(x, y, window = SurvArea)
  xy <- cbind(x,y)
#  plot(pointdata, pch = 19, cex = .5, add = TRUE)

  lower.x.lim <- SurvArea$xrange[1]
  upper.x.lim <- SurvArea$xrange[2]
  lower.y.lim <- SurvArea$yrange[1]
  upper.y.lim <- SurvArea$yrange[2]
  GridPts <- pointSimSyst(nrow = 211, ncol = 211,
	  lower.x.lim = lower.x.lim,
	  upper.x.lim = upper.x.lim,
	  lower.y.lim = lower.y.lim,
	  upper.y.lim = upper.y.lim)
  GridPtsPPP <- ppp(GridPts[,1], GridPts[,2], window = SurvArea)
  GridPtsPPP <- GridPtsPPP[SurvArea]
  predgrd <- cbind(GridPtsPPP$x, GridPtsPPP$y)
  NpredgrdOriginal <- length(predgrd[,1])
#  plot(SurvArea)
#  points(predgrd, pch = 19, cex = .3)

  SpatialCountData <- NULL
  i <- 2
  for(i in 1:length(wsamples$bdry)) {
    x <- wsamples$bdry[[i]]$x
    y <- wsamples$bdry[[i]]$y
    nx <- length(x)
    x <- (x - xmean.fix)/xystdv.fix
    y <- (y - ymean.fix)/xystdv.fix
    polybound <- cbind(x,y)
    TempPlot <- owin(poly = list(x = x, y = y))
    Area <- TempPlot$bdry[[1]]$area
    ipout <- inpip(xy, polybound)
    predin <- inout(predgrd, polybound)
    predgrd <- predgrd[!predin,]
    cnt <- length(ipout)
    centroidx <- centroid.owin(TempPlot)$x
    centroidy <- centroid.owin(TempPlot)$y
    SpatialCountData <- rbind(SpatialCountData, data.frame(centroidx = centroidx,
      centroidy = centroidy, Area = Area, Count = cnt, Density = cnt/Area))
#    plot(TempPlot, add = TRUE)
  }
  NpredgrdFinal <- length(predgrd[,1])
  ProportionUnsurveyed <- NpredgrdFinal/NpredgrdOriginal

  MaxDens.fix <- max(SpatialCountData[,"Density"])
  SpatialCountData[,"StdDens"] <- SpatialCountData[,"Density"]/MaxDens.fix

# ------------------------------------------------------------------------------
# end standardize x and y coordinates
# ------------------------------------------------------------------------------


  fCnt5 <- floor(.03*length(SpatialCountData[,"Count"]))
  fCnt5 <- max(fCnt5,1)
  fCnt10 <- floor(.05*length(SpatialCountData[,"Count"]))
  fCnt10 <- max(fCnt10,1)
  fCnt20 <- floor(.08*length(SpatialCountData[,"Count"]))
  fCnt20 <- max(fCnt20,1)
  fCnt30 <- floor(.12*length(SpatialCountData[,"Count"]))
  fCnt30 <- max(fCnt30,1)

  iter.time <- Sys.time()
#  plot(c(0,2),c(0,2), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
#  text(1,1.5, paste("Simulation", iter), cex = 3)
#  text(1,0.5, paste("Elapsed Time", iter.time - Start.Time), cex = 2)

  EstOut <- isppifac1(SurvAreaBoundaryClassOwin = SurvArea,
    PlotCountsDataFrame = SpatialCountData, DensVarName  = "StdDens",
    CountVarName = "Count", AreaVarName = "Area",
    xColName = "centroidx", yColName = "centroidy",
    nNodesRequestC = max(ceiling(fCnt5/5),2), nNodesRequestF = min(floor(4*fCnt5/5),4), 
		ScaleFactorDens = MaxDens.fix,
    PredictionGrid = predgrd, ProportionUnsurveyed = ProportionUnsurveyed,
    PercentZero = 75)

  EstOut1 <- isppifac1(SurvAreaBoundaryClassOwin = SurvArea,
    PlotCountsDataFrame = SpatialCountData, DensVarName  = "StdDens",
    CountVarName = "Count", AreaVarName = "Area",
    xColName = "centroidx", yColName = "centroidy",
    nNodesRequestC = max(ceiling(fCnt10/5),2), nNodesRequestF = floor(4*fCnt10/5),
		ScaleFactorDens = MaxDens.fix,
    PredictionGrid = predgrd, ProportionUnsurveyed = ProportionUnsurveyed,
    PercentZero = 75)

  EstOut2 <- isppifac1(SurvAreaBoundaryClassOwin = SurvArea,
    PlotCountsDataFrame = SpatialCountData, DensVarName  = "StdDens",
    CountVarName = "Count", AreaVarName = "Area",
    xColName = "centroidx", yColName = "centroidy",
    nNodesRequestC = ceiling(fCnt20/5), nNodesRequestF = floor(4*fCnt20/5),
		ScaleFactorDens = MaxDens.fix,
    PredictionGrid = predgrd, ProportionUnsurveyed = ProportionUnsurveyed,
    PercentZero = 75)

  EstOut3 <- isppifac1(SurvAreaBoundaryClassOwin = SurvArea,
    PlotCountsDataFrame = SpatialCountData, DensVarName  = "StdDens",
    CountVarName = "Count", AreaVarName = "Area",
    xColName = "centroidx", yColName = "centroidy",
    nNodesRequestC = ceiling(fCnt30/5), nNodesRequestF = floor(4*fCnt30/5),
		ScaleFactorDens = MaxDens.fix,
    PredictionGrid = predgrd, ProportionUnsurveyed = ProportionUnsurveyed,
    PercentZero = 75)
    
# ------------------------------------------------------------------------------
# classical random sampling
# ------------------------------------------------------------------------------


  MultFact <- sum(SpatialCountData[,"Area"])/
    (1-ProportionUnsurveyed)
  SRSEst <- mean(SpatialCountData[,"Density"])*MultFact
  SRSSE <- sqrt(var(SpatialCountData[,"Density"])*MultFact^2/
    length(SpatialCountData[,1]))*ProportionUnsurveyed
    
  StoreSims <- rbind(StoreSims, cbind(data.frame(TrueAbun = TrueAbundance),
    SPP5.Est = EstOut$Estimate, SPP5.StdErrSpp = EstOut$StdErrSpp,
    SPP5.StdErrGLMM = EstOut$StdErrGLMM, SPP5.ODTrad = EstOut$OverDispFactorTrad,
    SPP5.ODMed = EstOut$OverDispFactorMedian, SPP5.ODGT0 = EstOut$OverDispFactorGT0,
    SPP5.ODPerc = EstOut$OverDispFactorPerc,
    SPP10.Est = EstOut1$Estimate, SPP10.StdErrSpp = EstOut1$StdErrSpp,
    SPP10.StdErrGLMM = EstOut1$StdErrGLMM, SPP10.ODTrad = EstOut1$OverDispFactorTrad,
    SPP10.ODMed = EstOut1$OverDispFactorMedian, SPP10.ODGT0 = EstOut1$OverDispFactorGT0,
    SPP10.ODPerc = EstOut1$OverDispFactorPerc,
    SPP20.Est = EstOut2$Estimate, SPP20.StdErrSpp = EstOut2$StdErrSpp,
    SPP20.StdErrGLMM = EstOut2$StdErrGLMM, SPP20.ODTrad = EstOut2$OverDispFactorTrad,
    SPP20.ODMed = EstOut2$OverDispFactorMedian, SPP20.ODGT0 = EstOut2$OverDispFactorGT0,
    SPP20.ODPerc = EstOut2$OverDispFactorPerc,
    SPP30.Est = EstOut3$Estimate, SPP30.StdErrSpp = EstOut3$StdErrSpp,
    SPP30.StdErrGLMM = EstOut3$StdErrGLMM, SPP30.ODTrad = EstOut3$OverDispFactorTrad,
    SPP30.ODMed = EstOut3$OverDispFactorMedian, SPP30.ODGT0 = EstOut3$OverDispFactorGT0,
    SPP30.ODPerc = EstOut3$OverDispFactorPerc,
    SRS.Est = SRSEst, SRS.StdErr = SRSSE))

  write.table(StoreSims, "StoreSims4.csv", quote = F, sep = ",", row.names = F)

}



