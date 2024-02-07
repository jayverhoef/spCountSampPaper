\name{SimSummary}
\alias{SimSummary}
\title{Summary statistics from simulated data
Summary statistics from simulated data}
\usage{
  SimSummary(StoreSims, RootName, ODType = NA,
    failCutEst = NA, failCutSE = NA, failCutOD = NA)
}
\arguments{
  \item{StoreSims}{isppifac results stored for simulated
  data}

  \item{RootName}{Root name for a column in StoreSims,
  either "SRS", "SPP5", "Spp10", "Spp20", or "Spp30"}

  \item{StdErrType}{Either "Spp"" or "GLMM""}

  \item{ODType}{Overdispersion type; either "Trad" or "Perc
  or Vary"}
}
\value{
  data.frame of summary statistics
}
\description{
  Summary statistics from simulated data Summary statistics
  from simulated data
}
\author{
  Jay Ver Hoef \email{jay.verhoef@noaa.gov}
}

