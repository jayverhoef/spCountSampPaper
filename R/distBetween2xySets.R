#' distance between two matrices, data.frames, or sp SpatialPoints

#' distance between two matrices, data.frames, or sp SpatialPoints

#' @param xy1 internal
#' @param xy2 internal

#' @return matrix of distances

#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

distBetween2xySets <- function(xy1, xy2)
{
	# total number of observations for each set of coordinates
		xy1 <- as.data.frame(xy1)
		xy2 <- as.data.frame(xy2)
    x1 <- xy1[,1]
    y1 <- xy1[,2]
    x2 <- xy2[,1]
    y2 <- xy2[,2]
		n1 <- length(x1)
		n2 <- length(x2)
	# expand all x-coordinates
		sx1 <- matrix(x1, ncol = 1) %*%
			matrix(rep(1,times = n2), nrow = 1)
		sx2 <- matrix(rep(1,times = n1), ncol = 1) %*%
			matrix(x2, nrow = 1)
		sy1 <- matrix(y1, ncol = 1) %*%
			matrix(rep(1, times = n2), nrow = 1)
		sy2 <- matrix(rep(1, times = n1), ncol = 1) %*%
			matrix(y2, nrow = 1)
	# take differences
		newx <- sx1 - sx2
		newy <- sy1 - sy2
	# compute distance for the scaled and rotated coordinates */
		sqrt(newx^2 + newy^2)
}


