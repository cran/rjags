\name{mcarray.object}
\alias{print.mcarray}
\alias{summary.mcarray}
\alias{as.mcmc.list.mcarray}
\title{Objects for representing MCMC output}
\description{
  An \code{mcarray} object is used by the \code{jags.samples} function
  to represent MCMC output from a JAGS model. It is an array with named
  dimensions, for which the dimensions "iteration" and "chain" have a
  special status
}
\usage{
\method{summary}{mcarray}(object, FUN, \ldots)
\method{print}{mcarray}(x, \ldots)
\method{as.mcmc.list}{mcarray}(x, \ldots)
}
\arguments{
  \item{object,x}{an \code{mcarray} object}
  \item{FUN}{a function to be used to generate summary statistics}
  \item{\ldots}{additional arguments to the call}
}
\value{
  The \code{summary} method for \code{mcarray} objects applies the
  given function to the array, marginalizing the "chain" and "iteration"
  dimensions.

  The \code{print} method applies the summary function with
  \code{FUN=mean}.

  The \code{as.mcmc.list} method coerces an \code{mcarray} to an
  \code{mcmc.list} object so that the diagnostics provided by the
  \code{coda} package can be applied to the MCMC output it represents.
}
\details{
  The \code{coda} package defines \code{mcmc} objects for representing
  output from an MCMC sampler, and \code{mcmc.list} for representing
  output from multiple parallel chains.  These objects emphasize the
  time-series aspect of the MCMC output, but lose the original array
  structure of the variables they represent.  The \code{mcarray} class
  attempts to rectify this by preserving the dimensions of the original
  node array defined in the JAGS model.
}
\author{Martyn Plummer}
\keyword{models}

