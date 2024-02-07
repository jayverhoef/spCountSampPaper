library(maptools)
library(spCountSamp)
setwd("/media/Hitachi2GB/00NMML/GlacierPhotoSampling/spCountSamp/spCountSamp/inst/data/Sim3")


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
    if (i != 3 & j != 2 & j != 5)
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

iter <- 1

StoreSims <- NULL

niter <- 1000

Start.Time <- Sys.time()
 for (iter in 1:niter) {
	set.seed(iter)
  lower.x.bbox <- runif(1, 3.5, 4.5)
  upper.x.bbox <- runif(1, 7.5, 8.5)
  lower.y.bbox <- runif(1, 3.5, 4.5)
  upper.y.bbox <- runif(1, 7.5, 8.5)
  nseed.big <- 100
  nseed.sma <- 25
  Poi.mean.big <- 15
  Poi.mean.sma <- 9
  big.range <- 1
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
  simPts1 <- Sim$SimPts

  simPts <- simPts1

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
		nNodesRequestC = max(ceiling(fCnt5/5),2), 
		nNodesRequestF = floor(4*fCnt5/5), percentZero = 75)

	EstOut2 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = max(ceiling(fCnt10/5),2), 
		nNodesRequestF = floor(4*fCnt10/5), percentZero = 75)

	EstOut3 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = max(ceiling(fCnt15/5),2), 
		nNodesRequestF = floor(4*fCnt15/5), percentZero = 75)

	EstOut4 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = max(ceiling(fCnt20/5),2), 
		nNodesRequestF = floor(4*fCnt20/5), percentZero = 75)
    
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
    SPP1.Est = EstOut1$estimate, SPP1.StdErrSpp = EstOut1$stdErrSpp,
    SPP1.StdErrGLMM = EstOut1$stdErrGLMM, SPP1.ODTrad = EstOut1$overDispFactorTrad,
    SPP1.ODMed = EstOut1$overDispFactorMedian, SPP1.ODGT0 = EstOut1$overDispFactorGT0,
    SPP1.ODPerc = EstOut1$overDispFactorPerc,
    SPP2.Est = EstOut2$estimate, SPP2.StdErrSpp = EstOut2$stdErrSpp,
    SPP2.StdErrGLMM = EstOut2$stdErrGLMM, SPP2.ODTrad = EstOut2$overDispFactorTrad,
    SPP2.ODMed = EstOut2$overDispFactorMedian, SPP2.ODGT0 = EstOut2$overDispFactorGT0,
    SPP2.ODPerc = EstOut2$overDispFactorPerc,
    SPP3.Est = EstOut3$estimate, SPP3.StdErrSpp = EstOut3$stdErrSpp,
    SPP3.StdErrGLMM = EstOut3$stdErrGLMM, SPP3.ODTrad = EstOut3$overDispFactorTrad,
    SPP3.ODMed = EstOut3$overDispFactorMedian, SPP3.ODGT0 = EstOut3$overDispFactorGT0,
    SPP3.ODPerc = EstOut3$overDispFactorPerc,
    SPP4.Est = EstOut4$estimate, SPP4.StdErrSpp = EstOut4$stdErrSpp,
    SPP4.StdErrGLMM = EstOut4$stdErrGLMM, SPP4.ODTrad = EstOut4$overDispFactorTrad,
    SPP4.ODMed = EstOut4$overDispFactorMedian, SPP4.ODGT0 = EstOut4$overDispFactorGT0,
    SPP4.ODPerc = EstOut4$overDispFactorPerc,
    SRS.Est = SRSEst, SRS.StdErr = SRSSE))

  write.table(StoreSims, "StoreSims3.csv", quote = F, sep = ",", row.names = F)

}

