#' Default summary method for spCountSamp objects

#' Default summary method for spCountSamp objects

#' @param ob spCountSamp objects

#' @return list of summary statistics, including the estimate, a data.frame
#' of various standard error estimates, a data.frame of proportions of the
#' variance components, and a data.frame of the range parameters.

#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @rdname summary
#' @method summary spCountSamp
#' @S3method summary spCountSamp

summary.spCountSamp <- function(ob)
{
	li <- list(
		Estimates = list(estimate = ob$estimate,
			stdError = data.frame(
				SE = ob$stdErr,
				SE.ODTrad = ob$stdErrOD,
				SE.ODTrimGlobal = ob$stdErrGT,
				SE.ODTrimLocal = ob$stdErrLT,
				SE.ODRegr = ob$stdErrLR)
		),
		rangeParameters = data.frame(
			coarseScale = ob$rangeC,
			fineScale = ob$rangeF
		),
		proportionSurveyed = ob$propSurveyed
	)
	class(li) <- "summary.spCountSamp"
	return(li)
}


