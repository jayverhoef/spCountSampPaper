#-------------------------------------------------------------------------------
#
#           addRGBClassLegend
#
#-------------------------------------------------------------------------------

#' Adds a RGB Classes 
#'
#' Adds a RGB class legend to the currently active plot
#'
#' @param xleft a scalar of the left x position.
#' @param ybottom: a scalar of bottom y position.
#' @param xright a scalar of the right x position.
#' @param ytop: a scalar of top y position.
#' @param rgblist: a list where each element is a 3-vector of the rgb-values for each class item.
#' @param labels: a vector of class labels of equal length to the rgblist.
#'
#' @seealso \code{\link{plotPointsRGB}}, \code{\link{rect}}, 

#' @return add a color ramp legend as a rectangle to the currently active plot
#'
#' @author Jay Ver Hoef
#' @rdname addBreakColorLegend
#' @export addBreakColorLegend 

addBreakColorLegend <- function(xleft, ybottom, xright, ytop, breaks, 
	colors, printFormat = "4.2", ...) 
{
	nshades <- length(colors)
	for(i in 1:nshades)
		rect(xleft, ybottom + (i-1)/nshades*(ytop - ybottom), xright, 
			ybottom + i/nshades*(ytop - ybottom), col = colors[i], border = NA)
	tickInc <- (ytop - ybottom)/nshades
	breakLabels <- sprintf(paste("%",as.character(printFormat),"f", 
		sep = ""), breaks)
	for(i in 1:nshades) 
		text(xright, ybottom + tickInc*(i-1), breakLabels[i], 
			pos = 4, ...)
	text(xright, ytop, breakLabels[nshades + 1], 
			pos = 4, ...)
	return(invisible())	
} 



