\name{pointSimSyst}
\alias{pointSimSyst}
\title{Creates a systematic grid of points}
\usage{
  pointSimSyst(nrow = 10, ncol = 10, lower.x.lim = -1,
    upper.x.lim = 1, lower.y.lim = -1, upper.y.lim = 1)
}
\arguments{
  \item{lower.x.lim}{the lower limit for x-coordinate}

  \item{upper.x.lim}{the upper limit for x-coordinate}

  \item{lower.y.lim}{the lower limit for y-coordinate}

  \item{upper.y.lim}{the upper limit for y-coordinate}

  \item{ncol}{the number of cols in the systematic grid}

  \item{nrow}{the number of rows in the systematic grid}
}
\value{
  A data.frame with x- and y-coordinates of simulated
  locations
}
\description{
  Creates a systematic grid of points
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

