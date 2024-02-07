#' Standardize the plot coordinates
#' 
#' Standardize the plot coordinates 
#'
#' @param plots as polygons of sp Class SpatialPolygons
#' @param xmean.fix mean for standardizing x coordinates
#' @param ymean.fix mean for standardizing y coordinates
#' @param xystdv.fix standard deviation for standardizing both x and y coordinates
#' 
#' @return a list, where the input plots have standarized coordinates in the plts item.
#' The original plot areas are returned as plts.originalarea item.  The values
#'  used for standardization are arguments xmean.fix, ymean.fix, and xystdv.fix.  Standardized
#' x values are computed as (x-xmean.fix)/xystdv.fix and standardized y values are 
#' computed as (y-xmean.fix)/xystdv.fix.
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

stdizePlots <- function(plots, xmean.fix, ymean.fix, xystdv.fix) {
	plts.originalarea <- NULL
	storage <- vector("list", length(plots@polygons))
	for(i in 1:length(plots@polygons)) {
		x <- plots@polygons[[i]]@Polygons[[1]]@coords[,1]
		y <- plots@polygons[[i]]@Polygons[[1]]@coords[,2]
		x <- (x - xmean.fix)/xystdv.fix
		y <- (y - ymean.fix)/xystdv.fix
		storage[[i]] <- Polygons(list(Polygon(cbind(x,y))), 
			ID = plots@polygons[[i]]@ID)
		plts.originalarea[i] <- plots@polygons[[i]]@Polygons[[1]]@area
	}
	plts <- SpatialPolygons(storage)
 
	df <- methods:::as(plots, "data.frame")
	plts <- SpatialPolygonsDataFrame(plts, df)
  plts@data[,'centroidX'] = sapply(slot(plts, "polygons"), 
    function(x) slot(x, "labpt"))[1,]
  plts@data[,'centroidY'] = sapply(slot(plts, "polygons"), 
    function(x) slot(x, "labpt"))[2,]
  plts@data[,'pltarea'] = sapply(slot(plts, "polygons"), 
    function(x) slot(x, "area"))

	list(plts = plts, plts.originalarea = plts.originalarea)
}


