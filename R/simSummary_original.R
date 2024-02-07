#' Summary statistics from simulated data

#' Summary statistics from simulated data

#' @param StoreSims isppifac results stored for simulated data
#' @param RootName Root name for a column in StoreSims, either "SRS", "SPP5",
#'		"Spp10", "Spp20", or "Spp30"
#' @param StdErrType Either "Spp"" or "GLMM""
#' @param ODType Overdispersion type; either "Trad" or "Perc or Vary"


#' @return data.frame of summary statistics

#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

SimSummary.original <- function(StoreSims, RootName, ODType = NA, 
	failCutEst = NA, failCutSE = NA, failCutOD = NA)
{
	nOrig <- length(StoreSims[,1])
	if(!is.na(failCutOD)) {
		ind <- !is.na(StoreSims[,paste("ODTrad",  RootName, sep = "")])
		StoreSims <- StoreSims[ind,]
	}

  EstCol <- paste("Est", RootName, sep = "")
	if(!is.na(ODType) & ODType == "OD") {
		StdErrCol <- paste("StdErrOD", RootName, sep = "")
		StdError <- StoreSims[,StdErrCol]
	} 
	if(!is.na(ODType) & ODType == "GT") {
		StdErrCol <- paste("StdErrGT",  RootName, sep = "")
		StdError <- StoreSims[,StdErrCol]
	}		
	if(!is.na(ODType) & ODType == "LR") {
		StdErrCol <- paste("StdErrLR",  RootName, sep = "")
		StdError <- StoreSims[,StdErrCol]
	}		
	if(!is.na(ODType) & ODType == "LT") {
		StdErrCol <- paste("StdErrLT",  RootName, sep = "")
		StdError <- StoreSims[,StdErrCol]
	} 
	if(is.na(ODType)) {
		StdErrCol <- paste("StdErr",  RootName, sep = "")
		StdError <- StoreSims[,StdErrCol]
	} 

	if(!is.na(failCutEst)) {
		ind <- StoreSims[,EstCol] < failCutEst
		StoreSims <- StoreSims[ind,]
		StdError <- StdError[ind]
	}
	if(!is.na(failCutSE)) {
		ind <- StdError < failCutSE
		StoreSims <- StoreSims[ind,]
		StdError <- StdError[ind]
	}
  Bias <- mean(StoreSims[,EstCol] - StoreSims[,"TrueAbun"])
  Biasmed <- median(StoreSims[,EstCol] - StoreSims[,"TrueAbun"])
  RMSPE <- sqrt(mean((StoreSims[,EstCol] - StoreSims[,"TrueAbun"])^2))
  RAEV <- sqrt(mean(StdError^2))
  CIcover95 <- sum(StoreSims[,"TrueAbun"] > (StoreSims[,EstCol] -
      qnorm(.975)*StdError) &
    StoreSims[,"TrueAbun"] < (StoreSims[,EstCol] +
      qnorm(.975)*StdError))/length(StoreSims[,1])
  CIcover90 <- sum(StoreSims[,"TrueAbun"] > (StoreSims[,EstCol] -
      qnorm(.95)*StdError) &
    StoreSims[,"TrueAbun"] < (StoreSims[,EstCol] +
      qnorm(.95)*StdError))/length(StoreSims[,1])
  CIcover80 <- sum(StoreSims[,"TrueAbun"] > (StoreSims[,EstCol] -
      qnorm(.9)*StdError) &
    StoreSims[,"TrueAbun"] < (StoreSims[,EstCol] +
      qnorm(.9)*StdError))/length(StoreSims[,1])

  data.frame(RootName = RootName, ODType = ODType,
    Bias = Bias, BiasMedian = Biasmed, RMSPE = RMSPE, RAEV = RAEV,
    CIcover95 = CIcover95, CIcover90 = CIcover90, CIcover80 = CIcover80,
		failRate = 1 - length(StdError)/nOrig)
}

