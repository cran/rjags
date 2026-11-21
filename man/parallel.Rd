\name{parallel}
\alias{parallel.seeds}
\title{Get initial values for parallel RNGs}
\description{
  On a multi-processor system, you may wish to run parallel chains using
  multiple \code{jags.model} objects, each running a single chain on a
  separate processor.  This function returns a list of values that may
  be used to initialize the random number generator of each chain.

}
\usage{
parallel.seeds(factory, nchain)
}
\arguments{
  \item{factory}{Name of the RNG factory to use.}
  \item{nchain}{Number of chains for which to initialize RNGs.}
}
\value{
  \code{parallel.seeds} returns a list of RNG states. Each element
  is a list of length 2 with the following elements:
  \item{.RNG.name}{The name of the RNG}
  \item{.RNG.state}{An integer vector giving the state of the RNG.}
}
\note{
  To make the results of \code{parallel.seeds} reproducible, the
  function \code{jags.seed} should be called first.
}
\examples{
##The BaseRNG factory generates up to four distinct types of RNG. If
##more than 4 chains are requested, it will recycle the RNG types, but
##use different initial values
parallel.seeds("base::BaseRNG", 4)

## The lecuyer module provides the RngStream factory, which allows large
## numbers of independent parallel RNGs to be generated. 
load.module("lecuyer")
list.factories(type="rng")
parallel.seeds("lecuyer::RngStream", 5);
}
\author{Martyn Plummer}
\seealso{
  \code{\link{jags.model}}, section \dQuote{Random number generators},
  for further details on RNG initialization;
  \code{\link{list.factories}} to find the names of available RNG
  factories.
  \code{\link{jags.seed}} to set the seed for the random number
  generators used by JAGS.
}
\keyword{models}
