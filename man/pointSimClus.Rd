\name{pointSimClus}
\alias{pointSimClus}
\title{Simulate a doubly clustered set of points}
\usage{
  pointSimClus(nseed.big = 1, nseed.sma = 40,
    Poi.mean.big = 100, Poi.mean.sma = 10, big.range = 3,
    sma.range = 0.2, lower.x.lim = 0, upper.x.lim = 10,
    lower.y.lim = 0, upper.y.lim = 10, lower.x.bbox = 3,
    upper.x.bbox = 7, lower.y.bbox = 3, upper.y.bbox = 7,
    trend = FALSE)
}
\arguments{
  \item{nseed.big}{number of parent seeds with big range.
  Default = 1.}

  \item{nseed.small}{number of parent seeds with small
  range. Default = 40.}

  \item{Poi.mean.big}{mean of Poisson for number of
  offspring points around parents seeded by nseed.big.
  Default = 100.}

  \item{Poi.mean.small}{mean of Poisson for number of
  offspring points around parents seeded by nseed.small.
  Default = 10.}

  \item{big.range}{radius around each parent seeded by
  nseed.big. All simulated offspring are uniformly
  simulated within this radius. Default = 3.}

  \item{sma.range}{radius around each parent seeded by
  nseed.small. All simulated offspring are uniformly
  simulated within this radius. Default = 0.2.}

  \item{lower.x.lim}{the lower limit for x-coordinate.
  Default = 0.}

  \item{upper.x.lim}{the upper limit for x-coordinate.
  Default = 10.}

  \item{lower.y.lim}{the lower limit for y-coordinate.
  Default = 0.}

  \item{upper.y.lim}{the upper limit for y-coordinate.
  Default = 10.}

  \item{trend}{include a trend on the clusters}
}
\value{
  A list where SimPts is data.frame with x- and
  y-coordinates of simulated locations and SimParms returns
  the parameters of the simulation.
}
\description{
  Simulate a doubly clustered set of points
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

