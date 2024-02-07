#' Abundance estimator from counts in irregularly spaced plot
#'
#' Creates an abundance estimator from counts in spatial plots by 
#' estimating a spatial point pattern intensity surface between observed
#' plots  and then integrating over the intensity surface to estimate
#' abundance for unsampled area.  It then adds to counted plots, thus it
#' has a finite population correction factor
#'
#' @param formula a formula like response ~ 1, where the response variable is
#' on the left.  Currently, this is the only form that is implemented; that is,
#' no covariates are allowed.  The response variable must be a column name in the
#' SpatialPolygonDataFrame of the plots argument.
#' @param outline an outline of a single polygon of sp Class SpatialPolygon
#' @param plots as polygons of sp Class SpatialPolygons
#' @param nNodesRequestC number of node requests at coarse scale
#' @param nNodesRequestF number of node requests at fine scale
#' @param PercentZero The percent of area with zeros to eliminate when
#' computing overdispersion factor, default = 0
#' @param nodeSetSeed random number seed for k-means computation of nodes

#' @return A list with a lot of items.

#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

spCountSamp <- function(formula, outline, plots, nNodesRequestC, nNodesRequestF,
	percentZero = 0, nodeSetSeed = 101)
{
	StartTime <- Sys.time()
	respVar <- as.character(attr(terms(formula),"variables"))[2]

	xmean.fix <- mean(outline@polygons[[1]]@Polygons[[1]]@coords[,1])
	ymean.fix <- mean(outline@polygons[[1]]@Polygons[[1]]@coords[,2])
	xystdv.fix <- mean(sqrt(var(outline@polygons[[1]]@Polygons[[1]]@coords[,1])),
		sqrt(var(outline@polygons[[1]]@Polygons[[1]]@coords[,2])))

	bnd.originalarea <- outline@polygons[[1]]@area

	#standardize boundary data
	stdizeBoundaryOut <- stdizeBoundary(outline, xmean.fix, ymean.fix, xystdv.fix)
	bnd <- stdizeBoundaryOut$bnd

	# standardize plot
	stdizePlotsOut <- stdizePlots(plots, xmean.fix, ymean.fix, xystdv.fix)
	plts <- stdizePlotsOut$plts

	# Create Prediction Grid
	createPredGridOut <- createPredGridCountSamp(bnd, plts, respVar, nNodesRequestC,
		nNodesRequestF, nodeSetSeed)
	GridPtsp <- createPredGridOut$GridPtsp
	nodesC <- createPredGridOut$nodesC
	nodesF <- createPredGridOut$nodesF
	convexPolyKnotsFine <- createPredGridOut$convexPolyKnotsFine
	convexPolyKnotsFineMat <- convexPolyKnotsFine@polygons[[1]]@Polygons[[1]]@coords
	convexPolyKnotsFineMat[,"x"] <- convexPolyKnotsFineMat[,"x"]*xystdv.fix + xmean.fix
	convexPolyKnotsFineMat[,"y"] <- convexPolyKnotsFineMat[,"y"]*xystdv.fix + ymean.fix
	convexPolyKnotsFine <- SpatialPolygons(list(Polygons(list(Polygon(convexPolyKnotsFineMat)), ID = 1)))
	# Create distance matrices among knots for starting values and parameter boundaries
	distCC <- distBetween2xySets(nodesC, nodesC)
	distFF <- distBetween2xySets(nodesF, nodesF)
	distC0 <- distCC
	diag(distC0) <- 1e+32
	rangeC <- min(distC0)*2
	distF0 <- distFF
	diag(distF0) <- 1e+32
	rangeF <- min(distF0)*2

	# minimize the minus log likelihood for range parameters
	minFrange  <- .5*min(distF0)
	maxFrange <- 3*min(distF0)
	maxCrange <- 3*min(distC0)
	distDC <- distBetween2xySets(plts@data[,c("centroidX","centroidY")], nodesC)
	distDF <- distBetween2xySets(plts@data[,c("centroidX","centroidY")], nodesF)
	mLL <- function(ranges, respVar, plts, distDC, distDF,
		minFrange, maxFrange, maxCrange) {
		rangeF <- minFrange + exp(ranges[1])/
			(1 + exp(ranges[1]))*(maxFrange - minFrange)
		rangeC <- maxFrange + exp(ranges[2])/
			(1 + exp(ranges[2]))*(maxCrange - maxFrange)
		GauKernDC <- exp(-(distDC/rangeC)^2)
		GauKernDF <- exp(-(distDF/rangeF)^2)
		X <- cbind(GauKernDC, GauKernDF)
		y <- plts@data[,respVar]
  	dat4IWLS <- as.data.frame(cbind(y,X))
		options(warn = -1)
		betaHat <- coef(glm(y ~ . - 1, family = poisson, 
			offset = log(plts@data[,"pltarea"]), data = dat4IWLS))
		options(warn = 0)
		sum(exp(X %*% betaHat) - y * X %*% betaHat)
	}
	parmest <- optim(c(log(rangeF), log(rangeC) - log(rangeF)), mLL, 
		respVar = respVar, plts = plts, distDC = distDC, distDF = distDF,
		minFrange = minFrange, maxFrange = maxFrange, maxCrange = maxCrange)
	
	# create fitted design matrices outside of mLL
	rangeF <- minFrange + exp(parmest$par[1])/
			(1 + exp(parmest$par[1]))*(maxFrange - minFrange)
	rangeC <- maxFrange + exp(parmest$par[2])/
			(1 + exp(parmest$par[2]))*(maxCrange - maxFrange)
	GauKernDC <- exp(-(distDC/rangeC)^2)
	GauKernDF <- exp(-(distDF/rangeF)^2)
	X <- cbind(GauKernDC, GauKernDF)

	# beta parameter estimates
	y <- plts@data[,respVar]
	dat4IWLS <- as.data.frame(cbind(y,X))
	options(warn = -1)
	betaHatF <- coef(glm(y ~ . - 1, offset = log(plts@data[,"pltarea"]),
		family = poisson, data = dat4IWLS))
	options(warn = 0)
	fits <- exp(X %*% betaHatF)
	SigF <- solve(covB(X, fits))/mean(plts@data[,"pltarea"])

	# create prediction design matrices
	distPC <- distBetween2xySets(GridPtsp, nodesC)
	distPF <- distBetween2xySets(GridPtsp, nodesF)
	GauKernPC <- exp(-(distPC/rangeC)^2)
	GauKernPF <- exp(-(distPF/rangeF)^2)
	XP <- cbind(GauKernPC, GauKernPF)

	#predictions for all unsampled area based on GridPtsp grid
	pred0F <- exp(XP %*% betaHatF)
	
	# Sampled area
	cB <- sum(plts@data[,"pltarea"])
	# Unsampled area
	cU <- sum(sapply(slot(bnd@polygons[[1]], "Polygons"), 
    function(x) slot(x, "area"))*
    (2*(1-sapply(slot(bnd@polygons[[1]], "Polygons"), 
    function(x) slot(x, "hole"))) - 1)) - cB

	# sum predictions * area per prediction + sum observed
	EstF <- mean(pred0F)*cU + sum(plts@data[,respVar])

	#scaled predictions
	predOrigAreaF <- pred0F*cU/
		(sum(((2*!stdizeBoundaryOut$bnd.originalarea$hole) - 1)*
      stdizeBoundaryOut$bnd.originalarea$area) - 
    sum(sapply(slot(plots, "polygons"), 
       function(x) slot(x, "area"))))
	predPerGridF <- pred0F*cU/
		length(GridPtsp@coords[,1])

	#variance estimators
	#partial derivative vector to be used in delta method
	cvecF <- apply(as.vector(pred0F)*XP,2,mean)*cU
	varEstF <- mean(pred0F)*cU + t(cvecF) %*% SigF %*% cvecF
	stdErrF <- sqrt(varEstF)

	# Overdispersion Estimators
	Cnt <- plts@data[,respVar]
	plta <- plts@data[,"pltarea"]
	fits0F <- fits*plta
	# traditional estimator
	ODtradF <- sum((Cnt - fits0F)^2/fits0F)/(length(Cnt) - length(X[1,]))
	ODtradF <- max(1,ODtradF)
	# trimmed OD estimator
	fitscutF <- quantile(fits0F, percentZero/100)
	IndFits1F <- fits0F >= fitscutF
	fits1F <- fits0F[IndFits1F]
	Cnt1F <- Cnt[IndFits1F]
	ODpercF <- sum((Cnt1F - fits1F)^2/fits1F)/(length(Cnt1F) - length(X[1,]))
	ODpercF <- max(1,ODpercF)
		# make it local
	predQ <- pred0F
	predQ[pred0F > fitscutF] <- pred0F[pred0F > fitscutF]*ODpercF
	fitsQ <- fits*0 + 1
	fitsQ[fits0F > fitscutF] <- ODpercF
	varEstQ <- mean(predQ)*cU + mean(fitsQ)*t(cvecF) %*% SigF %*% cvecF
	stdErrQ <- sqrt(varEstQ)
	# regression estimator
	fits0R <- fits0F
	fits0R[fits0F < 1e-150] <- 1e-150
	ODFdata <- data.frame(y = (Cnt - fits0R)^2, x = fits0R)
#	ODvaryR <- lm(y ~ x - 1, data = ODFdata, weights = 1/fits0R)$coefficients
	ODvaryR <- max(lm(y ~ x - 1, data = ODFdata, 
		weights = sqrt(fits0R))$coefficients,1)
#	ODvaryR <- max(lm(y ~ x - 1, data = ODFdata)$coefficients,1)
	varEstR <- mean(pred0F*ODvaryR)*cU + 
		ODvaryR*t(cvecF) %*% SigF %*% cvecF
	stdErrR = sqrt(varEstR)
	
	EndTime <- Sys.time()
	 
	spCntSmp <- list(
		estimate = EstF, 
		stdErr = stdErrF, 
		stdErrOD = stdErrF*sqrt(ODtradF),
		stdErrGT = stdErrF*sqrt(ODpercF),
		stdErrLR = stdErrR,
		stdErrLT = stdErrQ,
		ODtrad = ODtradF, 
		ODtrim = ODpercF,
		percentZero = percentZero,
		propSurveyed = cB/(cU + cB),
		rangeF = rangeF*xystdv.fix, 
		rangeC = rangeC*xystdv.fix,
		outline = outline,
		fits = data.frame(fitsFixed = fits0F), 
		plots = plots,
		IntensityF = data.frame(x = as.data.frame(GridPtsp)[,1]*xystdv.fix +
			xmean.fix, y = as.data.frame(GridPtsp)[,2]*xystdv.fix + ymean.fix, 
			Intensity = predOrigAreaF),
		PredictionsF = data.frame(x = as.data.frame(GridPtsp)[,1]*xystdv.fix +
			xmean.fix, y = as.data.frame(GridPtsp)[,2]*xystdv.fix + ymean.fix, 
			Predictions = predPerGridF),
		startTime = StartTime,
		elapsedTime = EndTime - StartTime,
		nNodesRequestC = nNodesRequestC, 
		nNodesRequestF = nNodesRequestF, 
		nodeLocationsC = data.frame(x = nodesC[,1]*xystdv.fix + xmean.fix, 
			y = nodesC[,2]*xystdv.fix + ymean.fix),
		nodeLocationsF = data.frame(x = nodesF[,1]*xystdv.fix + xmean.fix, 
			y = nodesF[,2]*xystdv.fix + ymean.fix),
		convexPolyKnotsFine = convexPolyKnotsFine,
    SigF = SigF,
    betaHatF = betaHatF,
    XP = XP,
    cU = cU,
    varEstFpart1 = mean(pred0F)*cU,
    varEstFpart2 = t(cvecF) %*% SigF %*% cvecF)

	class(spCntSmp) <- "spCountSamp"
	spCntSmp

}

