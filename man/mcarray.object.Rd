\name{mcarray.object}
\alias{mcarray.object}
\alias{print.mcarray}
\alias{summary.mcarray}
\alias{as.mcmc.list.mcarray}
\title{Objects for representing MCMC output}
\description{
  An \code{mcarray} object is used by the \code{jags.samples} function
  to represent monitored output from a JAGS model. It is a tagged array with
  tags "value", "iteration" and "chain".
}
\usage{
\method{summary}{mcarray}(object, FUN, \ldots)
\method{print}{mcarray}(x, \ldots)
\method{as.mcmc.list}{mcarray}(x, na.rm=TRUE, \ldots)
}
\arguments{
  \item{object,x}{an \code{mcarray} object}
  \item{FUN}{a function to be used to generate summary statistics}
  \item{na.rm}{indicates whether elements with missing values should be removed}
  \item{\ldots}{additional arguments to the call}
}
\value{
  The \code{summary} method for \code{mcarray} objects collapses
  the object over "chain" and "iteration" dimensions leaving "value"
  dimensions.

  The \code{print} method prints the mean, collapsing over
  "chain" and "iteration" dimensions. It also includes information
  about the "stat" and "summary" of the JAGS monitor and the size
  of the collapsed dimensions.

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

  Objects of class \code{mcarray} are tagged arrays with a limited
  set of supported tags:
  \describe{
    \item{"value"}{dimensions corresponding to the dimension of the object
      in the JAGS model}
    \item{"iteration"}{dimension corresponding to the evolution of the
      Markov chain over iterations}
    \item{"chain"}{dimension corresponding to replication of the Markov
      chain over parallel chains}
  }
  There can be multiple dimensions with tag "value" but at most one with
  tag "iteration" and at most one with tag "chain".
}
\author{Martyn Plummer}
\keyword{models}
\seealso{\code{\link{collapse}}}
