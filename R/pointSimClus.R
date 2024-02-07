#' Simulate a doubly clustered set of points
#'
#' Simulate a doubly clustered set of points 
#'
#' @param nseed.big number of parent seeds with big range. Default = 1.
#' @param nseed.small number of parent seeds with small range. Default = 40.
#' @param Poi.mean.big mean of Poisson for number of offspring points around parents seeded by nseed.big. Default = 100.
#' @param Poi.mean.small mean of Poisson for number of offspring points around parents seeded by nseed.small. Default = 10.
#' @param big.range radius around each parent seeded by nseed.big. All simulated offspring are uniformly simulated within this radius. Default = 3.
#' @param sma.range radius around each parent seeded by nseed.small. All simulated offspring are uniformly simulated within this radius. Default = 0.2.
#' @param lower.x.lim the lower limit for x-coordinate. Default = 0.
#' @param upper.x.lim the upper limit for x-coordinate. Default = 10.
#' @param lower.y.lim the lower limit for y-coordinate. Default = 0.
#' @param upper.y.lim the upper limit for y-coordinate. Default = 10.
#' @param trend include a trend on the clusters

#' @return A list where SimPts is data.frame with x- and y-coordinates of simulated locations and SimParms returns the parameters of the simulation.
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

pointSimClus <-
function(
	nseed.big = 1,
	nseed.sma = 40,
	Poi.mean.big = 100,
	Poi.mean.sma = 10,
	big.range = 3,
  sma.range = .2,
	lower.x.lim = 0, upper.x.lim = 10,
	lower.y.lim = 0, upper.y.lim = 10,
	lower.x.bbox = 3, upper.x.bbox = 7,
	lower.y.bbox = 3, upper.y.bbox = 7,
  trend = FALSE)
{
	store.results <- data.frame(x = NULL, y = NULL)
	x.range <- upper.x.lim - lower.x.lim
	y.range <- upper.y.lim - lower.y.lim
	store.rand.parms <- NULL
	if(nseed.big > 0) {
	  for(i in 1:nseed.big){
      x.seed <- lower.x.lim + runif(1, lower.x.bbox/10, upper.x.bbox/10)*x.range
      y.seed <- lower.y.lim + runif(1, lower.y.bbox/10, upper.y.bbox/10)*y.range
      n.per.seed <- rpois(1, lambda = Poi.mean.big)
      if(Poi.mean.big == 0) n.per.seed <- 1
      sim.seed.range <- runif(1, 0.1, 0.1 + 2*big.range)
      for(j in 1:n.per.seed){
       rand.x <- x.seed + runif(1,-1,1)*sim.seed.range
       rand.y <- y.seed + runif(1,-1,1)*sim.seed.range
       z <- runif(1)
       trendsurf <- .5*(rand.x/10) + .5*(rand.y/10)
	     store.results <- rbind(store.results,
		     data.frame(x = rand.x, y = rand.y, z = z, trend = trendsurf))
      }
      store.rand.parms <- rbind(store.rand.parms,
        matrix(c(x.seed, y.seed, n.per.seed, sim.seed.range), nrow = 1))
	  }
	}
	if(nseed.sma > 0) {
	  for(i in 1:nseed.sma){
      x.seed <- lower.x.lim + runif(1, lower.x.bbox/10, upper.x.bbox/10)*x.range
      y.seed <- lower.y.lim + runif(1, lower.y.bbox/10, upper.y.bbox/10)*y.range
      n.per.seed <- rpois(1, lambda = Poi.mean.sma)
      if(Poi.mean.sma == 0) n.per.seed <- 1
      sim.seed.range <- runif(1, 0.01, 0.01 + 2*sma.range)
      for(j in 1:n.per.seed){
       rand.x <- x.seed + runif(1,-1,1)*sim.seed.range
       rand.y <- y.seed + runif(1,-1,1)*sim.seed.range
       z <- runif(1)
       trendsurf <- .5*(rand.x/10) + .5*(rand.y/10)
	     store.results <- rbind(store.results,
		     data.frame(x = rand.x, y = rand.y, z = z, trend = trendsurf))
      }
      store.rand.parms <- rbind(store.rand.parms,
        matrix(c(x.seed, y.seed, n.per.seed, sim.seed.range), nrow = 1))
	  }
	}
  store.results <- store.results[store.results[,1] > lower.x.lim,]
  store.results <- store.results[store.results[,1] < upper.x.lim,]
  store.results <- store.results[store.results[,2] > lower.y.lim,]
  store.results <- store.results[store.results[,2] < upper.y.lim,]
  if(trend == TRUE) {
    store.results <- store.results[store.results[,3] < store.results[,4],]
  }
	list(SimPts = store.results[,1:2], SimParms = store.rand.parms)
}

