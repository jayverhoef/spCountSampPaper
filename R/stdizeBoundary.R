#' Standardize the boundary coordinates
#' 
#' Standardize the boundary coordinates 
#'
#' @param outline an outline of a single polygon of sp Class SpatialPolygon
#' @param xmean.fix mean for standardizing x coordinates
#' @param ymean.fix mean for standardizing y coordinates
#' @param xystdv.fix standard deviation for standardizing both x and y coordinates
#'
#' @return a list, where the input outline has standarized coordinates in the bnd item.
#' The boundary area after standardization is returned as bnd.area item.  The values
#  used for standardization are items xmean.fix, ymean.fix, and xystdv.fix.  Standardized
#' x values are computed as (x-xmean.fix)/xystdv.fix and standardized y values are 
#' computed as (y-xmean.fix)/xystdv.fix.
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

stdizeBoundary <- function(outline, xmean.fix, ymean.fix, xystdv.fix) {
	bnd.originalarea <- NULL
  bnd.hole = NULL
	storage <- vector("list", length(plots@polygons[[1]]@Polygons))
	for(i in 1:length(outline@polygons[[1]]@Polygons)) {
		x <- outline@polygons[[1]]@Polygons[[i]]@coords[,1]
		y <- outline@polygons[[1]]@Polygons[[i]]@coords[,2]
		x <- (x - xmean.fix)/xystdv.fix
		y <- (y - ymean.fix)/xystdv.fix
		storage[[i]] <- Polygon(cbind(x,y), 
      hole = outline@polygons[[1]]@Polygons[[i]]@hole) 
		bnd.originalarea <- c(bnd.originalarea, 
      outline@polygons[[1]]@Polygons[[i]]@area)
    bnd.hole <- c(bnd.hole, outline@polygons[[1]]@Polygons[[i]]@hole)
	}
  plts = Polygons(storage, ID = 1)
	bnd <- SpatialPolygons(list(plts))

	list(bnd = bnd, bnd.originalarea = data.frame(area = bnd.originalarea,
    hole = bnd.hole))
}

