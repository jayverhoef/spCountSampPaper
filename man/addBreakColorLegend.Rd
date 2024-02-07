\name{addBreakColorLegend}
\alias{addBreakColorLegend}
\title{Adds a RGB Classes}
\usage{
  addBreakColorLegend(xleft, ybottom, xright, ytop, breaks,
    colors, printFormat = "4.2", ...)
}
\arguments{
  \item{xleft}{a scalar of the left x position.}

  \item{ybottom:}{a scalar of bottom y position.}

  \item{xright}{a scalar of the right x position.}

  \item{ytop:}{a scalar of top y position.}

  \item{rgblist:}{a list where each element is a 3-vector
  of the rgb-values for each class item.}

  \item{labels:}{a vector of class labels of equal length
  to the rgblist.}
}
\value{
  add a color ramp legend as a rectangle to the currently
  active plot
}
\description{
  Adds a RGB class legend to the currently active plot
}
\author{
  Jay Ver Hoef
}
\seealso{
  \code{\link{plotPointsRGB}}, \code{\link{rect}},
}

