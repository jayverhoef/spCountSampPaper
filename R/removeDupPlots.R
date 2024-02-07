removeDupPlots = function(plts)
{
  centroidX = sapply(slot(plts, "polygons"), 
    function(x) slot(x, "labpt"))[1,]
  centroidY = sapply(slot(plts, "polygons"), 
    function(x) slot(x, "labpt"))[2,]
  xy <- SpatialPoints(cbind(centroidX, centroidY))
  i = 10
  repBefore = NULL
  for(i in 2:length(plts)) 
  {
    indx = 1:(i - 1)
    repBefore = c(repBefore, over(xy[i,], SpatialPolygons(plts@polygons)[indx,]))
  }
  plts[c(TRUE,is.na(repBefore)),]
}

