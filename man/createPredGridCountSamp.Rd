\name{createPredGridCountSamp}
\alias{createPredGridCountSamp}
\title{Creates grid of prediction points within boundary but outside of plots}
\usage{
  createPredGridCountSamp(bnd, plts, respVar,
    nNodesRequestC, nNodesRequestF, nodeSetSeed)
}
\arguments{
  \item{bnd}{an outline of a single polygon of sp Class
  SpatialPolygon}

  \item{plts}{plots as polygons of sp Class
  SpatialPolygons}

  \item{respVar}{name of response variable}

  \item{nNodesRequestC}{number of coarse nodes}

  \item{nNodesRequestF}{number of fine nodes}
}
\value{
  a list, where the GridPtsp item contains the prediction
  grid as a of the coarse nodes, and nodesF item is a
  data.frame with x,y-coordinates of the fine nodes.
}
\description{
  Creates grid of prediction points within boundary but
  outside of plots. Also computes coarse and fine node
  points using kmeans on subsamples of grid points.
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

