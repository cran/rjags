\name{jags.seed}
\alias{jags.seed}
\title{Set initial value for JAGS RNGs}
\description{
  Sets the random seed used by JAGS.
}
\usage{
jags.seed(seed)
}
\arguments{
  \item{seed}{A single integer value.}
}
\details{
  This function sets the seed for the pseudo-random number generators
  (RNGs) used by JAGS making subsequent simulations reproducible.

  Once the random seed is set by \code{jags.seed}, the same seed will be
  applied to RNG factories in subsequently loaded modules. In practical
  terms this means that a call to \code{jags.seed} may come before or
  after a call to \code{load.module} and will have the same effect.
}
\note{
  The JAGS RNGs are independent of the RNGs used by R. A call to the
  base function \code{set.seed} has no effect on the RNGs used by
  JAGS, and vice versa.
}
\author{Martyn Plummer}
\seealso{
  \code{\link{list.factories}} to find the names of available RNG
  factories.
}
\keyword{models}
