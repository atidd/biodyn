% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/biodyn-plots.R
\docType{methods}
\name{plot,biodyn,missing-method}
\alias{plot,biodyn,missing-method}
\alias{plot,biodyns,missing-method}
\title{plot}
\usage{
\S4method{plot}{biodyn,missing}(x, y, probs = c(0.95, 0.75, 0.5, 0.25, 0.05),
  na.rm = FALSE, type = 7, worm = NULL, fn = list(Stock = function(x)
  stock(x), Harvest = function(x) harvest(x), Yield = function(x) catch(x)),
  facet = facet_wrap(~qname, scales = "free", ncol = 1), ...)
}
\arguments{
\item{x}{an object of class \code{biodyn}}

\item{y}{second argument}

\item{probs}{numeric vector of probabilities with values in [0,1]. (Values up to 2e-14 outside that range are accepted and moved to the nearby endpoint.)}

\item{na.rm}{FALSE}

\item{type}{an integer between 1 and 9 selecting one of the nine quantile algorithms detailed below to be used.}

\item{worm}{iters}

\item{fn}{functions}

\item{facet}{facet for panels}
}
\value{
an \code{ggplot2} object
}
\description{
Creates a \code{ggplot2} object that plots time series of biomass, harvest rate and catch. The basic object can then be modified by adding ggplot2 layers.
}
\examples{
\dontrun{

x =sim()
x =window(x,end=49)
bd1=fwd(x,harvest=rlnorm(200,log(harvest(x)[,-1]),.2))
bd2=fwd(x,harvest=rlnorm(200,log(harvest(x)[,-1])*1.5,.2))
plot(biodyns("1"=bd1,"2"=bd2))
}
}
\seealso{
\code{\link{plotPrd}}, \code{\link{plotEql}}
}

