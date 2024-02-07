#' Default image method for spCountSamp objects.
#' 
#' Default plotting method for spCountSamp objects.  Maps the prediction surface as a raster grid after converting it to a SpatialPixelsDataFrame object. 
#'
#' @param ob fitted model of class spCountSamp
#' 
#' @return an image plot. The \code{SpatialPolygon} of the outline and \code{SpatialPolygonsDataFrame} of the sample plots can be added.  Other arguments such as \code{col} and \code{breaks} can be passed to the \code{\link{image}} function from package \code{sp}.
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @rdname image
#' @method image spCountSamp
#' @S3method image spCountSamp

image.spCountSamp <- function(ob, ...)
{
	preds <- ob$Predictions
	coordinates(preds) <- c("x","y")
	pgrid <- SpatialPixelsDataFrame(preds,ob$Predictions)
	image(pgrid,"Predictions", ...)
}

