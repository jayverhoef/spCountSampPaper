library(maptools)
library(spCountSamp)
library(rgeos)
setwd("/media/Hitachi2GB/00NMML/RPackages/spCountSamp_package/spCountSamp/inst/data/Sim1")


#-------------------------------------------------------------------------------
#  SAMPLE PLOTS
#-------------------------------------------------------------------------------

ni <- 16
nj <- 16
PlotSize <- 0.3
offst <- (10-.3*16)/(2*17)
ep <- .01
samples <- NULL
ID <- 1
for(i in 1:ni) {
  for (j in 1:nj) {
#    if (i != 3 & j != 2 & j != 5)
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
plot(plots)


loXlim <- 0
upXlim <- 10
loYlim <- 0
upYlim <- 10
outline <- SpatialPolygons(list(Polygons(list(Polygon(
	cbind(c(loXlim, upXlim, upXlim, loXlim, loXlim),
	c(loYlim, loYlim, upYlim, upYlim, loYlim)))), ID = "bnd")))
plot(outline, add = TRUE)

#-------------------------------------------------------------------------------
#  SIMULATION
#-------------------------------------------------------------------------------

StoreSims <- NULL

niter <- 1000
Start.Time <- Sys.time()
 for (iter in 1:niter) {
	set.seed(iter)
  lower.x.bbox <- 0
  upper.x.bbox <- 10
  lower.y.bbox <- 0
  upper.y.bbox <- 10
  nseed.big <- 2000
  nseed.sma <- 0
  Poi.mean.big <- 0
  Poi.mean.sma <- 0
  big.range <- .00000001
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
  simPts1 <- Sim$SimPts

  simPts <- simPts1

	coordinates(simPts) <- c("x","y")

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


  fCnt5 <- floor(.05*length(plots@polygons))
  fCnt5 <- max(fCnt5,1)
  fCnt10 <- floor(.10*length(plots@polygons))
  fCnt10 <- max(fCnt10,1)
  fCnt15 <- floor(.15*length(plots@polygons))
  fCnt15 <- max(fCnt15,1)
  fCnt20 <- floor(.20*length(plots@polygons))
  fCnt20 <- max(fCnt20,1)

  iter.time <- Sys.time()
  plot(c(0,2),c(0,2), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  text(1,1.5, paste("Simulation", iter), cex = 3)
  text(1,0.5, paste("Elapsed Time", iter.time - Start.Time), cex = 2)

	EstOut1 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 3, 
		nNodesRequestF = 8, percentZero = 75)

	EstOut2 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 4, 
		nNodesRequestF = 14, percentZero = 75)

	EstOut3 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 5, 
		nNodesRequestF = 20, percentZero = 75)

	EstOut4 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 6, 
		nNodesRequestF = 26, percentZero = 75)
    
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
    Est1 = EstOut1$estimate, StdErr1 = EstOut1$stdErr,
    ODTrad1 = EstOut1$ODtrad, ODtrim1 = EstOut1$ODtrim,
		StdErrOD1 = EstOut1$stdErrOD, StdErrGT1 = EstOut1$stdErrGT,
		StdErrLR1 = EstOut1$stdErrLR, StdErrLT1 = EstOut1$stdErrLT, 
    Est2 = EstOut2$estimate, StdErr2 = EstOut2$stdErr,
    ODTrad2 = EstOut2$ODtrad, ODtrim2 = EstOut2$ODtrim,
		StdErrOD2 = EstOut2$stdErrOD, StdErrGT2 = EstOut2$stdErrGT,
		StdErrLR2 = EstOut2$stdErrLR, StdErrLT2 = EstOut2$stdErrLT, 
    Est3 = EstOut3$estimate, StdErr3 = EstOut3$stdErr,
    ODTrad3 = EstOut3$ODtrad, ODtrim3 = EstOut1$ODtrim,
		StdErrOD3 = EstOut3$stdErrOD, StdErrGT3 = EstOut3$stdErrGT,
		StdErrLR3 = EstOut3$stdErrLR, StdErrLT3 = EstOut3$stdErrLT, 
    Est4 = EstOut4$estimate, StdErr4 = EstOut4$stdErr,
    ODTrad4 = EstOut4$ODtrad, ODtrim4 = EstOut4$ODtrim,
		StdErrOD4 = EstOut4$stdErrOD, StdErrGT4 = EstOut4$stdErrGT,
		StdErrLR4 = EstOut4$stdErrLR, StdErrLT4 = EstOut4$stdErrLT, 
    EstSRS = SRSEst, StdErrSRS = SRSSE))

  write.table(StoreSims, "StoreSims1_130505.csv", quote = F, sep = ",", row.names = F)

}

