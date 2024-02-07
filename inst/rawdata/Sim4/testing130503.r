library(maptools)
library(spCountSamp)
setwd("/home/jay/RPackages/spCountSamp_package/spCountSamp/inst/data/Sim4")


#-------------------------------------------------------------------------------
#  SAMPLE PLOTS
#-------------------------------------------------------------------------------

ni <- 26
nj <- 26
PlotSize <- 0.21
offst <- (10-.21*25)/(2*26)
ep <- .01
i <- 1
j <- 1
samples <- NULL
ID <- 1
for(i in 1:ni) {
  for (j in 1:nj) {
    if (i != 2 & i != 4 & j != 3 & j != 6)
    {
        xL <- (i-1)*10/ni + offst
        xU <- (i-1)*10/ni + offst + PlotSize
        yL <- (j-1)*10/nj + offst
        yU <- (j-1)*10/nj + offst + PlotSize
        samples <- c(samples, Polygons(list(Polygon(cbind(c(xL, xU, xU, xL, xL),
                                          c(yL, yL, yU, yU, yL)))), ID = ID))
				ID = ID + 1
    }
  }
}
plots <- SpatialPolygons(samples)
df <- data.frame(pltarea = rep(NA, times = length(plots@polygons)))
for(i in 1:length(plots@polygons)) 
	df[i,"pltarea"] <- plots@polygons[[i]]@Polygons[[1]]@area
row.names(df) <- as.character(1:length(plots@polygons))
plots <- SpatialPolygonsDataFrame(plots, df)
#plot(plots)


loXlim <- 0
upXlim <- 10
loYlim <- 0
upYlim <- 10
outline <- SpatialPolygons(list(Polygons(list(Polygon(
	cbind(c(loXlim, upXlim, upXlim, loXlim, loXlim),
	c(loYlim, loYlim, upYlim, upYlim, loYlim)))), ID = "bnd")))
#plot(outline, add = TRUE)

#-------------------------------------------------------------------------------
#  SIMULATION
#-------------------------------------------------------------------------------

StoreSims <- NULL
iter <- 2
	set.seed(iter)
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

  simPts <- rbind(SimPts1, SimPts2)

	coordinates(simPts) <- c("x","y")
#	plot(simPts, add = TRUE, pch = 19, cex = .5)

  TrueAbundance <- length(coordinates(simPts)[,1])

	counts <- rep(NA, times  = length(plots@polygons))
	for(i in 1:length(plots@polygons)) {
		counts[i] <- sum(!is.na(over(simPts, 
			SpatialPolygons(list(plots@polygons[[i]])))))
	}
	# add count data to Photo Plot Outlines
	pltsData <- plots@data
	pltsData[,"counts"] <- counts
	plots@data <- pltsData

# ------------------------------------------------------------------------------
# end standardize x and y coordinates
# ------------------------------------------------------------------------------

  fCnt1 <- floor(.02*length(plots@polygons))
  fCnt1 <- max(fCnt1,1)
  fCnt2 <- floor(.05*length(plots@polygons))
  fCnt2 <- max(fCnt2,1)
  fCnt3 <- floor(.10*length(plots@polygons))
  fCnt3 <- max(fCnt3,1)
  fCnt4 <- floor(.15*length(plots@polygons))
  fCnt4 <- max(fCnt4,1)

  iter.time <- Sys.time()
  plot(c(0,2),c(0,2), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  text(1,1.5, paste("Simulation", iter), cex = 3)
  text(1,0.5, paste("Elapsed Time", iter.time - Start.Time), cex = 2)

	EstOut1 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 3, 
		nNodesRequestF = 8, percentZero = 75)

	EstOut2 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 5, 
		nNodesRequestF = 16, percentZero = 75)

	EstOut3 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 7, 
		nNodesRequestF = 24, percentZero = 75)

	#undebug(spCountSamp)
	EstOut4 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 9, 
		nNodesRequestF = 32, percentZero = 75)

# summary(EstOut4)
    
# ------------------------------------------------------------------------------
# classical random sampling
# ------------------------------------------------------------------------------


  FPCF <- (1-EstOut4$propSurveyed)
  SRSEst <- mean(plots@data[,"counts"]/plots@data[,"pltarea"])*
		outline@polygons[[1]]@Polygons[[1]]@area
  SRSSE <- sqrt(var(plots@data[,"counts"]/plots@data[,"pltarea"]))*
		outline@polygons[[1]]@Polygons[[1]]@area*sqrt(FPCF)/
    sqrt(length(plots@data[,"pltarea"]))
    
  StoreSims <- rbind(StoreSims, data.frame(TrueAbun = TrueAbundance,
    EstF1 = EstOut1$estimateF, StdErrF1 = EstOut1$stdErrF,
    ODTradF1 = EstOut1$ODtradF, ODPercF1 = EstOut1$ODpercF,
		StdErrFV1 = EstOut1$stdErrV, 
    EstF2 = EstOut2$estimateF, StdErrF2 = EstOut2$stdErrF,
    ODTradF2 = EstOut2$ODtradF, ODPercF2 = EstOut2$ODpercF,
 		StdErrFV2 = EstOut2$stdErrV,
    EstF3 = EstOut3$estimateF, StdErrF3 = EstOut3$stdErrF,
    ODTradF3 = EstOut3$ODtradF, ODPercF3 = EstOut3$ODpercF,
		StdErrFV3 = EstOut3$stdErrV, 
    EstF4 = EstOut4$estimateF, StdErrF4 = EstOut4$stdErrF,
    ODTradF4 = EstOut4$ODtradF, ODPercF4 = EstOut4$ODpercF,
		StdErrFV4 = EstOut4$stdErrV,
    EstSRS = SRSEst, StdErrSRS = SRSSE))

  write.table(StoreSims, "StoreSims4_130505.csv", quote = F, sep = ",", row.names = F)



#library(spCountSamp)
#setwd("/media/Hitachi2GB/00NMML/RPackages/spCountSamp_package/spCountSamp/inst/data/Sim4")
#undebug(SimSummary)
SimSummary(read.csv("StoreSims4_130503.csv"),"F1","Vary")
#SimSummary(read.csv("StoreSims4_130503.csv"),"SRS")

