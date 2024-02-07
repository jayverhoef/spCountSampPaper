#' Computes part of the variance covariance matrix for fixed effects 
#' 
#' Computes part of the variance covariance matrix for fixed effects 
#'
#' @param X design matrix
#' @param fits fits
#'
#' @return a matrix 
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

covB <- function(X, fits)
{
	covBeta <- matrix(0, nrow = length(X[1,]), ncol = length(X[1,]))
	for(i in 1:length(X[,1])) {
		covBeta <- covBeta + outer(X[i,],X[i,])*fits[i]
	}

	return(covBeta)
}


