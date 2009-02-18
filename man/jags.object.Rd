\name{jags.object}
\alias{update.jags}
\alias{coef.jags}
\title{Functions for manipulating jags model objects}
\description{
  A \code{jags} object represents a Bayesian graphical model
  described using the BUGS language. 
}
\usage{
\method{update}{jags}(object, n.iter=1, by, \ldots)
\method{coef}{jags}(object, chain=1, \ldots)
}
\arguments{
  \item{object}{a \code{jags} model object}
  \item{n.iter}{number of iterations to update the model}
  \item{by}{update frequency for progress bar.  See Details}
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
\details{
  
  Since MCMC calculations are typically long, a progress bar is displayed
  during the call to \code{update}. The progress bar is refreshed every
  \code{by} iterations. If R is not being used interactively then the
  progress bar is suppressed.

}
\author{Martyn Plummer}
\keyword{models}
