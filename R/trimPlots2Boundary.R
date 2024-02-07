trimPlots2Boundary = function(plots, outline)
{
  centroidX = sapply(slot(plots, "polygons"), 
    function(x) slot(x, "labpt"))[1,]
  centroidY = sapply(slot(plots, "polygons"), 
    function(x) slot(x, "labpt"))[2,]
  xy <- SpatialPoints(cbind(centroidX, centroidY))
  xy@proj4string = plots@proj4string
  plts = plots[!is.na(over(xy, outline)[,1]),]
  plts
}


