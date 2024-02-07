\name{spCountSamp}
\alias{spCountSamp}
\title{Abundance estimator from counts in irregularly spaced plot}
\usage{
  spCountSamp(formula, outline, plots, nNodesRequestC,
    nNodesRequestF, percentZero = 0, nodeSetSeed = 101)
}
\arguments{
  \item{formula}{a formula like response ~ 1, where the
  response variable is on the left.  Currently, this is the
  only form that is implemented; that is, no covariates are
  allowed.  The response variable must be a column name in
  the SpatialPolygonDataFrame of the plots argument.}

  \item{outline}{an outline of a single polygon of sp Class
  SpatialPolygon}

  \item{plots}{as polygons of sp Class SpatialPolygons}

  \item{nNodesRequestC}{number of node requests at coarse
  scale}

  \item{nNodesRequestF}{number of node requests at fine
  scale}

  \item{PercentZero}{The percent of area with zeros to
  eliminate when computing overdispersion factor, default =
  0}

  \item{nodeSetSeed}{random number seed for k-means
  computation of nodes}
}
\value{
  A list with a lot of items.
}
\description{
  Creates an abundance estimator from counts in spatial
  plots by estimating a spatial point pattern intensity
  surface between observed plots and then integrating over
  the intensity surface to estimate abundance for unsampled
  area.  It then adds to counted plots, thus it has a
  finite population correction factor
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

