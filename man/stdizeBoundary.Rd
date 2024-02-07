\name{stdizeBoundary}
\alias{stdizeBoundary}
\title{Standardize the boundary coordinates}
\usage{
  stdizeBoundary(outline, xmean.fix, ymean.fix, xystdv.fix)
}
\arguments{
  \item{outline}{an outline of a single polygon of sp Class
  SpatialPolygon}

  \item{xmean.fix}{mean for standardizing x coordinates}

  \item{ymean.fix}{mean for standardizing y coordinates}

  \item{xystdv.fix}{standard deviation for standardizing
  both x and y coordinates}
}
\value{
  a list, where the input outline has standarized
  coordinates in the bnd item. The boundary area after
  standardization is returned as bnd.area item.  The values
  x values are computed as (x-xmean.fix)/xystdv.fix and
  standardized y values are computed as
  (y-xmean.fix)/xystdv.fix.
}
\description{
  Standardize the boundary coordinates
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

