\name{image.spCountSamp}
\alias{image.spCountSamp}
\title{Default image method for spCountSamp objects.}
\usage{
  \method{image}{spCountSamp} (ob, ...)
}
\arguments{
  \item{ob}{fitted model of class spCountSamp}
}
\value{
  an image plot. The \code{SpatialPolygon} of the outline
  and \code{SpatialPolygonsDataFrame} of the sample plots
  can be added.  Other arguments such as \code{col} and
  \code{breaks} can be passed to the \code{\link{image}}
  function from package \code{sp}.
}
\description{
  Default plotting method for spCountSamp objects.  Maps
  the prediction surface as a raster grid after converting
  it to a SpatialPixelsDataFrame object.
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

