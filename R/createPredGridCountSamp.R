#' Creates grid of prediction points within boundary but outside of plots
#' 
#' Creates grid of prediction points within boundary but outside of plots. Also
#' computes coarse and fine node points using kmeans on subsamples of grid points. 
#'
#' @param bnd an outline of a single polygon of sp Class SpatialPolygon
#' @param plts plots as polygons of sp Class SpatialPolygons
#' @param respVar name of response variable
#' @param nNodesRequestC number of coarse nodes
#' @param nNodesRequestF number of fine nodes
#'
#' @return a list, where the GridPtsp item contains the prediction grid as a  
#  SpatialPoints class.  The nodesC item is a data.frame with x,y-coordinates
#' of the coarse nodes, and nodesF item is a data.frame with x,y-coordinates 
#' of the fine nodes.
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

createPredGridCountSamp <- function(bnd, plts, respVar, nNodesRequestC, nNodesRequestF, 
	nodeSetSeed) {
	lower.x.lim <- bnd@bbox["x","min"]
	upper.x.lim <- bnd@bbox["x","max"]
	lower.y.lim <- bnd@bbox["y","min"]
	upper.y.lim <- bnd@bbox["y","max"]
	xrange <- upper.x.lim - lower.x.lim
	yrange <- upper.y.lim - lower.y.lim
	nc <- round(sqrt(xrange/yrange)*200)
	nr <- round(nc*yrange/xrange)
	GridPts <- pointSimSyst(nrow = nr, ncol = nc,
		lower.x.lim = lower.x.lim,
		upper.x.lim = upper.x.lim,
		lower.y.lim = lower.y.lim,
		upper.y.lim = upper.y.lim)
	GridPtsp <- SpatialPoints(GridPts)
	# create grid within boundary
	GridPtsp <- GridPtsp[!is.na(sp:::over(GridPtsp, bnd))]
	set.seed(nodeSetSeed)
	# create course node locations
	nodesC <- kmeans(as.data.frame(GridPtsp), nNodesRequestC, 
		nstart = 20, iter.max = 500, algorithm="Lloyd")$centers
	# create fine node locations
	centroids.gt0 <- NULL
	for(i in 1:length(plots@polygons))
		if(plts@data[i,respVar] > 0) centroids.gt0 <- rbind(centroids.gt0,
			plts@polygons[[i]]@labpt)
	centroids.gt0 <- SpatialPoints(centroids.gt0)
	poly.gt0 <- gConvexHull(centroids.gt0)
	nodesF <- kmeans(as.data.frame(GridPtsp[!is.na(sp:::over(GridPtsp, poly.gt0))]),
		nNodesRequestF, nstart = 20, iter.max = 500, algorithm="Lloyd")$centers
	# create grid outside of observed plots
	GridPtsp <- GridPtsp[is.na(over(GridPtsp, plts)[,respVar])]
	list(GridPtsp = GridPtsp, nodesC = nodesC, nodesF = nodesF, 
		convexPolyKnotsFine = poly.gt0)
}

