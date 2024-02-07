#' Default plotting method for spCountSamp objects.  Maps the prediction surface.
#' 
#' Default plotting method for spCountSamp objects.  Maps the prediction surface. 
#'
#' @param ob fitted model of class spCountSamp
#' @param cex.leglab character expansion for legend symbols and characters
#' @param main same as for par
#' @param cex.main character expansion for title main
#' 
#' @return a plot. The map of the outline, sample plots, and prediction surface.
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

plot.spCountSamp <- function(ob, cex.leglab = 1, cex.main = 1, 
	main = "Study Area")
{
	par.orig <- par(no.readonly = TRUE)
	layout(matrix(c(1,2), nrow = 1), widths = c(4,1))
	par(mar = c(0,0,5,0))
	plot(ob$outline)
	title(main = list(main, cex = cex.main))
	plot(ob$plots, add = TRUE)
	obPreds <- ob$Predictions
	qtiles <- quantile(obPreds[,"Predictions"], p = (1:9)/10)
	breaks <- c(min(obPreds[,"Predictions"])-1e-10, 
		qtiles, max(obPreds[,"Predictions"]))
	cramp <- rainbow(length(breaks) - 1, start = .66, end = .99)
	for(i in 1:(length(breaks) - 1)) {
		indbrk <- breaks[i] < obPreds[,"Predictions"] & 
			obPreds[,"Predictions"] <= breaks[i + 1]
		obPtemp <- obPreds[indbrk,]
		coordinates(obPtemp) <- c("x","y")
		plot(obPtemp, add = TRUE, pch = 19, cex = .4, col = cramp[i])
	}
	par(mar = c(0,0,0,0))
	plot(c(0,1),c(0,1), type = "n", xaxt = "n", yaxt = "n", bty = "n")
	breaks[1] <- min(obPreds[,"Predictions"])
	legend(0,.6, legend = rep("", times = 10), pch = rep(19, times = 10), 
		col = cramp, bty = "n", cex = cex.leglab)
	legend(-.09,.615, legend = as.character(format(breaks, scientific = TRUE, 
		digits = 3)), cex = cex.leglab, bty = "n")
	par(par.orig)
}





