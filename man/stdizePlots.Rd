\name{stdizePlots}
\alias{stdizePlots}
\title{Standardize the plot coordinates}
\usage{
  stdizePlots(plots, xmean.fix, ymean.fix, xystdv.fix)
}
\arguments{
  \item{plots}{as polygons of sp Class SpatialPolygons}

  \item{xmean.fix}{mean for standardizing x coordinates}

  \item{ymean.fix}{mean for standardizing y coordinates}

  \item{xystdv.fix}{standard deviation for standardizing
  both x and y coordinates}
}
\value{
  a list, where the input plots have standarized
  coordinates in the plts item. The original plot areas are
  returned as plts.originalarea item.  The values used for
  standardization are arguments xmean.fix, ymean.fix, and
  xystdv.fix.  Standardized x values are computed as
  (x-xmean.fix)/xystdv.fix and standardized y values are
  computed as (y-xmean.fix)/xystdv.fix.
}
\description{
  Standardize the plot coordinates
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

