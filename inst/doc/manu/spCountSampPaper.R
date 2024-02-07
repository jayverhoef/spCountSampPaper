
## ----setup, include=FALSE------------------------------------------------
# smaller font size for chunks
opts_chunk$set(size = 'tiny')


## ----real-example-code, echo=FALSE, include = FALSE, cache = TRUE--------
		library(spCountSampPaper)
    library(sp)
    library(maptools)
		library(xtable)
		path <- system.file("rawdata/seals", package = "spPlotSampCourse")
		outlineFile <- paste(path,"/","outline", sep = "")
		outline <- readShapePoly(outlineFile)
		plotsFile <- paste(path,"/","plots", sep = "")
		plots <- readShapePoly(plotsFile)

		# number of photos
		nPlots = length(plots@data[,1])
    sumCounts = sum(plots@data$counts)
    nPos = sum(plots@data$counts > 0)
    maxCounts = max(plots@data$counts)
		# area surveyed in km^2
		islandArea <- 0
		for(j in 2:11)
			islandArea <- islandArea + outline@polygons[[1]]@Polygons[[j]]@area
		totalArea <- outline@polygons[[1]]@Polygons[[1]]@area - islandArea
		#  Run the function
		#undebug(spCountSamp)
    startTime = Sys.time()
    sCSout <- spCountSamp(counts ~ 1, outline, plots, 
		  nNodesRequestC = 4, nNodesRequestF = 15, 
		  percentZero = 50, nodeSetSeed = 101)
    endTime = Sys.time()
    compTime = difftime(endTime, startTime, units = "secs")
    propSurv = 100*(summary(sCSout)$proportionSurveyed)
    rhoC = summary(sCSout)$rangeParameters[1]/1000
    rhoF = summary(sCSout)$rangeParameters[2]/1000
    totEst = summary(sCSout)$Estimates$estimate
    totSE = summary(sCSout)$Estimates$stdError$SE
    omegaOD = sCSout$stdErrOD/sCSout$stdErr
    omegaWR = sCSout$stdErrLR/sCSout$stdErr
    omegaTG = sCSout$stdErrGT/sCSout$stdErr
    omegaTL = sCSout$stdErrLT/sCSout$stdErr
    p1 = as.numeric(sCSout$varEstFpart1)
    p2 = as.numeric(sCSout$varEstFpart2)
#   summary(sCSout)



## ----echo = FALSE, include = FALSE---------------------------------------
		library(spCountSampPaper)
		library(xtable)
		path1 <- system.file("rawdata/Sim1", package = "spCountSampPaper")
		file.Sim1 <- paste(path1,"/","StoreSims1_130505.csv", sep = "")
		StoreSims1 <- read.csv(file.Sim1)
 		path2 <- system.file("rawdata/Sim2", package = "spCountSampPaper")
		file.Sim2 <- paste(path2,"/","StoreSims2_130505.csv", sep = "")
		StoreSims2 <- read.csv(file.Sim2)
		path3 <- system.file("rawdata/Sim3", package = "spCountSampPaper")
		file.Sim3 <- paste(path3,"/","StoreSims3_130505.csv", sep = "")
		StoreSims3 <- read.csv(file.Sim3)
		path4 <- system.file("rawdata/Sim4", package = "spCountSampPaper")
		file.Sim4 <- paste(path4,"/","StoreSims4_130505.csv", sep = "")
		StoreSims4 <- read.csv(file.Sim4)


## ----Table1, echo=FALSE, results = 'asis'--------------------------------
resultsSim1 <- cbind(
as.numeric(c(SimSummary(StoreSims1, "SRS")[,c("Bias","RMSPE","CIcover90")],NA,NA,NA,NA,0)),
as.numeric(c(
	SimSummary(StoreSims1, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims1, "1","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "1","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "1","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "1","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims1, "2", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims1, "2","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "2","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "2","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "2","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "2", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims1, "3", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims1, "3","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "3","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "3","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "3","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "3", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims1, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims1, "4","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "4","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "4","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "4","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims1, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
))
)
resultsSim1 <- as.data.frame(resultsSim1)
colnames(resultsSim1) <- c("SRS","knots1","knots2","knots3","knots4")
rownames(resultsSim1) <- c("Bias", "RMSPE","CI90$^a$","CI90$_{OD}^b$","CI90$_{WR}^c$","CI90$_{TG}^d$","CI90$_{TL}^e$","Fail Rate$^f$")
print(xtable(resultsSim1, digits = 3), type = "latex",
	tabular.environment = "tabular",
	include.colnames = FALSE,
	sanitize.rownames.function = function(x){x},
	only.contents = TRUE,
	hline.after = NULL)


## ----Table2, echo=FALSE, results = 'asis'--------------------------------
resultsSim2 <- cbind(
as.numeric(c(SimSummary(StoreSims2, "SRS")[,c("Bias","RMSPE","CIcover90")],NA,NA,NA,NA,0)),
as.numeric(c(
	SimSummary(StoreSims2, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims2, "1","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "1","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "1","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "1","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims2, "2", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims2, "2","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "2","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "2","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "2","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "2", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims2, "3", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims2, "3","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "3","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "3","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "3","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "3", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims2, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims2, "4","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "4","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "4","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "4","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims2, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
))
)
resultsSim2 <- as.data.frame(resultsSim2)
colnames(resultsSim2) <- c("SRS","knots1","knots2","knots3","knots4")
rownames(resultsSim2) <- c("Bias", "RMSPE","CI90$^a$","CI90$_{OD}^b$","CI90$_{WR}^c$","CI90$_{TG}^d$","CI90$_{TL}^e$","Fail Rate$^f$")
print(xtable(resultsSim2, digits = 3), type = "latex",
	tabular.environment = "tabular",
	include.colnames = FALSE,
	sanitize.rownames.function = function(x){x},
	only.contents = TRUE,
	hline.after = NULL)


## ----Table3, echo=FALSE, results = 'asis'--------------------------------
resultsSim3 <- cbind(
as.numeric(c(SimSummary(StoreSims3, "SRS")[,c("Bias","RMSPE","CIcover90")],NA,NA,NA,NA,0)),
as.numeric(c(
	SimSummary(StoreSims3, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims3, "1","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "1","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "1","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "1","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims3, "2", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims3, "2","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "2","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "2","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "2","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "2", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims3, "3", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims3, "3","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "3","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "3","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "3","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "3", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims3, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims3, "4","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "4","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "4","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "4","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims3, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
))
)
resultsSim3 <- as.data.frame(resultsSim3)
colnames(resultsSim3) <- c("SRS","knots1","knots2","knots3","knots4")
rownames(resultsSim3) <- c("Bias", "RMSPE","CI90$^a$","CI90$_{OD}^b$","CI90$_{WR}^c$","CI90$_{TG}^d$","CI90$_{TL}^e$","Fail Rate$^f$")
print(xtable(resultsSim3, digits = 3), type = "latex",
	tabular.environment = "tabular",
	include.colnames = FALSE,
	sanitize.rownames.function = function(x){x},
	only.contents = TRUE,
	hline.after = NULL)


## ----Table4, echo=FALSE, results = 'asis'--------------------------------
resultsSim4 <- cbind(
as.numeric(c(SimSummary(StoreSims4, "SRS")[,c("Bias","RMSPE","CIcover90")],NA,NA,NA,NA,0)),
as.numeric(c(
	SimSummary(StoreSims4, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims4, "1","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "1","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "1","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "1","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "1", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims4, "2", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims4, "2","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "2","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "2","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "2","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "2", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims4, "3", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims4, "3","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "3","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "3","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "3","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "3", failCutEst = 2000)[,"failRate"]
)),
as.numeric(c(
	SimSummary(StoreSims4, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(StoreSims4, "4","OD", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "4","LR", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "4","GT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "4","LT", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"CIcover90"],
	SimSummary(StoreSims4, "4", failCutEst = 2000, failCutSE = 300, failCutOD = TRUE)[,"failRate"]
))
)
resultsSim4 <- as.data.frame(resultsSim4)
colnames(resultsSim4) <- c("SRS","knots1","knots2","knots3","knots4")
rownames(resultsSim4) <- c("Bias", "RMSPE","CI90$^a$","CI90$_{OD}^b$","CI90$_{WR}^c$","CI90$_{TG}^d$","CI90$_{TL}^e$","Fail Rate$^f$")
print(xtable(resultsSim4, digits = 3), type = "latex",
	tabular.environment = "tabular",
	include.colnames = FALSE,
	sanitize.rownames.function = function(x){x},
	only.contents = TRUE,
	hline.after = NULL)


## ----FigRegionsSamples, fig.width=10, fig.height=3.5, echo=FALSE, dev = "tikz", include = FALSE, cache = TRUE----
	layout(matrix(1:3, nrow = 1))
	par(mar = c(0,1,8,3))
	plot(c(0,1),c(0,1), type = "n", xlab = "", ylab ="", xaxt = "n", yaxt = "n", bty = "n")
	rect(0,0,1,1,lwd = 2)
	mtext("A", 3, cex = 3, line = 1.5, adj = 0, padj = 0.2)
	rect(0.1666667, 0.1666667, 1 - 0.1666667, 1 - 0.1666667, lty = 5, lwd = 5)
	text(.06,.93,"$R$", cex = 3)
	text(.23,.76,"$A$", cex = 3)
	set.seed(12)
	points(runif(20), runif(20), pch = 19, cex = 2, col = rgb(.2,.2,.2))
	points(runif(1), runif(1), pch = 1, cex = 2)
	text(.58,.68,"$Y(\\bs_i)$", cex = 2)
	text(.73,.52,"$Y(\\bs_0)$", cex = 2)
	plot(c(0,1),c(0,1), type = "n", xlab = "", ylab ="", xaxt = "n", yaxt = "n", bty = "n")
	rect(0,0,1,1,lwd = 2)
	mtext("B", 3, cex = 3, line = 1.5, adj = 0, padj = 0.2)
	rect(0.1666667, 0.1666667, 1 - 0.1666667, 1 - 0.1666667, lty = 5, lwd = 7)
	for(i in 1:5) lines(c(i/6,i/6),c(0,1), lwd = 2)
	for(i in 1:5) lines(c(0,1),c(i/6,i/6), lwd = 2)
	rect(1*(1/6),3*(1/6),2*(1/6),4*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	text(1.5*(1/6),3.5*(1/6),"$B_i$", cex = 2.5)
	rect(2*(1/6),2*(1/6),3*(1/6),3*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(3*(1/6),3*(1/6),4*(1/6),4*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(1*(1/6),5*(1/6),2*(1/6),6*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(4*(1/6),1*(1/6),5*(1/6),2*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(5*(1/6),4*(1/6),6*(1/6),5*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(0*(1/6),0*(1/6),1*(1/6),1*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	plot(c(0,1),c(0,1), type = "n", xlab = "", ylab ="", xaxt = "n", yaxt = "n", bty = "n")
	rect(0,0,1,1,lwd = 2)
	mtext("C", 3, cex = 3, line = 1.5, adj = 0, padj = 0.2)
	rect(0.1666667, 0.1666667, 1 - 0.1666667, 1 - 0.1666667, lty = 5, lwd = 7)
	rect(1.5*(1/6),3*(1/6),2.5*(1/6),4*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	text(2*(1/6),3.5*(1/6),"$B_i$", cex = 2.5)
	rect(1.9*(1/6),1.2*(1/6),2.9*(1/6),2.2*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(3*(1/6),3.5*(1/6),4*(1/6),4.5*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(.8*(1/6),4.6*(1/6),1.8*(1/6),5.6*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(4.5*(1/6),.5*(1/6),5.5*(1/6),1.5*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(4.7*(1/6),3.8*(1/6),5.7*(1/6),4.8*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(0.2*(1/6),0.2*(1/6),1.2*(1/6),1.2*(1/6), lwd = 5, col = rgb(.8,.8,.8))
	rect(0.1666667, 0.1666667, 1 - 0.1666667, 1 - 0.1666667, lty = 5, lwd = 7)


## ----studyArea, fig.width=8, fig.height=8, echo=FALSE, include = FALSE, cache = TRUE----
		library(spPlotSampCourse)
		path <- system.file("rawdata/seals", package = "spPlotSampCourse")
		outlineFile <- paste(path,"/","outline", sep = "")
		outline <- readShapePoly(outlineFile)
		plotsFile <- paste(path,"/","plots", sep = "")
		plots <- readShapePoly(plotsFile)
		par(mar = c(0,0,0,0))
		plotsub <- plots[plots@data[,"counts"] > 0,]
		plot(outline)
		qtiles <- quantile(plotsub@data$counts, p = (1:3)/4)
		breaks <- c(min(plotsub@data$counts)-1e-10, 
			qtiles, max(plotsub@data$counts))
    breaks = c(1,2,3,6,44)
#		cramp <- c(rgb(.8,.8,.8), rgb(.6,.6,.6), rgb(.4,.4,.4), rgb(.2,.2,.2))
    cramp <- c('#fef0d9', '#fdcc8a', '#fc8d59', '#d7301f')
		ob <- plotsub
		colorCol <- "counts"
		plot(plots, add = TRUE)
    plot(plotsub[plotsub@data$counts > breaks[1] & plotsub@data$counts <= breaks[2], ],
			col = cramp[1], add = TRUE, border = par("bg"))
		plot(plotsub[plotsub@data$counts > breaks[2] & plotsub@data$counts <= breaks[3], ],
			col = cramp[2], add = TRUE, border = par("bg"))
		plot(plotsub[plotsub@data$counts > breaks[3] & plotsub@data$counts <= breaks[4], ],
			col = cramp[3], add = TRUE, border = par("bg"))
		plot(plotsub[plotsub@data$counts > breaks[4] & plotsub@data$counts <= breaks[5], ],
			col = cramp[4], add = TRUE, border = par("bg"))
		addBreakColorLegend(682464, 1181494, 684511, 1189428, 
			breaks = breaks, colors = cramp, printFormat = "2.0", cex = 1.1)
		SpatialPolygonsRescale(layout.scale.bar(), offset = c(688240, 1181567),
			scale = 5000, fill = c("transparent","black"), plot.grid = FALSE)
		text(688203,1182408,"0", cex = 1.5)
		text(693175,1182408,"5 km", cex = 1.5)
		SpatialPolygonsRescale(layout.north.arrow(), offset = c(697562,1193085),
			scale = 2000, col = rgb(.5,.5,.5), plot.grid = FALSE)


## ----simulationExample, fig.width=6, fig.height=6, echo=FALSE, include = FALSE, cache = TRUE----
		#  SAMPLE PLOTS
			library(spCountSampPaper)
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
			par(mar = c(0,0,8,0))
			plot(plots)
			loXlim <- 0
			upXlim <- 10
			loYlim <- 0
			upYlim <- 10
			outline <- SpatialPolygons(list(Polygons(list(Polygon(
				cbind(c(loXlim, upXlim, upXlim, loXlim, loXlim),
				c(loYlim, loYlim, upYlim, upYlim, loYlim)))), ID = "bnd")))
			plot(outline, add = TRUE)
		#  SIMULATION
			set.seed(32)
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
			simPts <- Sim$SimPts
			coordinates(simPts) <- c("x","y")
			plot(simPts, add = TRUE, pch = 19, cex = .5, col = rgb(.4,.4,.4))

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
			#undebug(spCountSamp)
			EstOut2 <- spCountSamp(counts ~ 1, outline, plots, 
				nNodesRequestC = 4, 
				nNodesRequestF = 14, percentZero = 75)

			points(EstOut2$nodeLocationsF, pch = 3, lwd = 3, cex = 3)
			points(EstOut2$nodeLocationsC, pch = 4, lwd = 3, cex = 3)
			plot(EstOut2$convexPolyKnotsFine, add = TRUE, lty = 2, lwd = 3)
			mtext("A", 3, cex = 3, line = 1.5, adj = .05, padj = 0.2)


## ----simulationPlotsFittedSurface, fig.width=6, fig.height=6, echo=FALSE, include = FALSE, fig.keep = 'last', cache = TRUE----
		library(spCountSampPaper)
		qtiles <- quantile(EstOut2$Predictions$Predictions, p = (1:7)/8)
		breaks <- c(min(EstOut2$Predictions$Predictions)-1e-32, 
			qtiles, max(EstOut2$Predictions$Predictions))
#		cramp <- NULL
#		for(i in 1:8) cramp <- c(cramp, rgb(i/9, i/9, i/9))
    cramp = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#990000')
		par(mar = c(0,0,8,0))
		layout(matrix(1:2, nrow = 1), widths = c(3,1))
		image(EstOut2, breaks = breaks, col = cramp)
		plot(plots, add = TRUE)
		plot(outline, add = TRUE)
		mtext("B", 3, cex = 3, line = 1.5, adj = .1, padj = 0.2)
		par(mar = c(0,0,8,0))
		plot(c(0,1), c(0,1), type = "n", xlab = "", ylab = "", bty = "n",
			xaxt = "n", yaxt = "n")
		addBreakColorLegend(.01, .2, .4, .8, 
			breaks = breaks, colors = cramp, printFormat = "2.4", cex = 1.4)


## ----PearResids4Overdisp, fig.width=6, fig.height=12, echo=FALSE, include = FALSE, cache = FALSE----
		fits <-	EstOut2$fits$fitsFixed
		par(mar = c(5,5,8,1))
		plot(fits, (counts - fits)^2/fits, pch = 1, cex = 1.5,
			xlab = "Fitted Value", ylab = "Squared Pearson Residuals",
			cex.lab = 2, cex.axis = 1.5)
		ODtrad <- sum((counts - fits)^2/fits)/(length(fits) - 19)
		lines(c(min(fits),max(fits)), c(ODtrad,ODtrad), lwd = 8)
		lines(c(min(fits),max(fits)), c(1,1), lwd = 4, lty = 2)
		x.75 <- quantile(fits, .75)
		ODtrim <- EstOut2$ODtrim
		lines(c(x.75,max(fits)), c(ODtrim,ODtrim), lwd = 8, lty = 5)
		mtext("A", 3, cex = 3, line = 1.5, adj = .01, padj = 0.2)


## ----rawResids4Overdisp, fig.width=6, fig.height=12, echo=FALSE, include = FALSE, cache = FALSE----
		par(mar = c(5,5,8,1))
		plot(fits, (counts - fits)^2, pch = 1, cex = 1.5,
			xlab = "Fitted Value", ylab = "Squared Raw Residuals",
			cex.lab = 2, cex.axis = 1.5)
		b <- coef(lm(y ~ x - 1, data = data.frame(x = fits, 
			y = (counts - fits)^2, weights = sqrt(fits))))
		abline(a = 0, b = b, lwd = 5)
		abline(a = 0, b = 1, lwd = 5, lty = 2)
		mtext("B", 3, cex = 3, line = 1.5, adj = .01, padj = 0.2)


## ----EffectTrimProp, fig.width=8, fig.height=6, echo=FALSE, include = FALSE, cache = TRUE----
		path <- system.file("rawdata/Sim5", package = "spCountSampPaper")
		file.Sim5 <- paste(path,"/","StoreSims5_130505.csv", sep = "")
		StoreSims5 <- read.csv(file.Sim5)
		sim5Results <- cbind((0:9)*.1,
			c(.825, SimSummary(StoreSims5, "1","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "2","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "3","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "4","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "5","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "6","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "7","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "8","GT")[,"CIcover90"],
			SimSummary(StoreSims5, "9","GT")[,"CIcover90"]),
			c(.825, SimSummary(StoreSims5, "1","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "2","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "3","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "4","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "5","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "6","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "7","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "8","LT")[,"CIcover90"],
			SimSummary(StoreSims5, "9","LT")[,"CIcover90"]))
		colnames(sim5Results) <- c("trimp","GT","LT")
		par(mar = c(5,5,1,1))
		plot(c(0,.9),c(min(sim5Results[,2:3]),max(sim5Results[,2:3])), type = "n",
			xlab = "Trim Proportion", ylab = "90% Confidence Interval Coverage", cex.axis = 1.4,
			cex.lab = 1.8)
		lines(sim5Results[,c("trimp","GT")], lwd = 4, col = 'green')
		points(sim5Results[,c("trimp","GT")], cex = 2)

		lines(c(0,.9), c(.9,.9), lty = 2, lwd = 4)
		lines(c(0,.9), c(.881,.881), lty = 3, lwd = 3)
		lines(c(0,.9), c(.919,.919), lty = 3, lwd = 3)
		legend(.01,.955, legend = c("Globally Adjusted", "Locally Adjusted"),
			lwd = c(3,3), pch = c(1,5), cex = 1.5)


## ----RealExampleIntSurfFit, fig.width=14, fig.height=11, echo=FALSE, include = FALSE, cache = TRUE----
		library(spCountSampPaper)
    library(sp)
    library(maptools)
		library(xtable)
		path <- system.file("rawdata/seals", package = "spPlotSampCourse")
		outlineFile <- paste(path,"/","outline", sep = "")
		outline <- readShapePoly(outlineFile)
		plotsFile <- paste(path,"/","plots", sep = "")
		plots <- readShapePoly(plotsFile)
    qtiles <- quantile(sCSout$Predictions$Predictions, p = (1:7)/8)
		breaks <- c(min(sCSout$Predictions$Predictions)-1e-32, 
			qtiles, max(sCSout$Predictions$Predictions))
		cramp <- NULL
		for(i in 1:8) cramp <- c(cramp, rgb(i/9, i/9, i/9))
    cramp = c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#990000')
		par(mar= c(0,0,0,0))
		image(sCSout, breaks = breaks, col = cramp)
		plot(plots, add = TRUE)
		points(sCSout$nodeLocationsF, pch = 3, lwd = 6, cex = 4)
		points(sCSout$nodeLocationsC, pch = 5, lwd = 6, cex = 4)
		plot(sCSout$convexPolyKnotsFine, add = TRUE, lty = 2, lwd = 6)
		plot(outline, add = TRUE)
		addBreakColorLegend(682464, 1181494, 684511, 1189428, 
			breaks = breaks, colors = cramp, printFormat = "2.4", cex = 2.0)
		SpatialPolygonsRescale(layout.scale.bar(), offset = c(688240, 1181567),
			scale = 5000, fill = c("transparent","black"), plot.grid = FALSE)
		text(688203,1182408,"0", cex = 2.5)
		text(693175,1182408,"5 km", cex = 2.5)
		SpatialPolygonsRescale(layout.north.arrow(), offset = c(697562,1193085),
			scale = 2000, col = rgb(.5,.5,.5), plot.grid = FALSE)


