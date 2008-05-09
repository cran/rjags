\name{jags.object}
\alias{update.jags}
\alias{coef.jags}
\title{Functions for manipulating jags model objects}
\description{
  A \code{jags} object represents a Bayesian graphical model
  described using the BUGS language. 
}
\usage{
\method{update}{jags}(object, niter=1, \ldots)
\method{coef}{jags}(object, chain=1, \ldots)
}
\arguments{
  \item{object}{a \code{jags} model object}
  \item{niter}{number of iterations to update the model}
  \item{chain}{chain number to query}
  \item{\ldots}{additional arguments to the call (ignored)}
}
\value{
  These are methods for generic functions applied to \code{jags}
  model objects.

  The \code{update} function update all samplers associated with the
  model by the given number of iterations. This function modifies the
  original object and invisibly returns \code{NULL}.

  The \code{coef} function returns a list with an entry for each Node
  array that contains an unobserved Node. Elements corresponding to
  observed Nodes or deterministic Nodes are given missing values.
}
\author{Martyn Plummer}
\keyword{models}

