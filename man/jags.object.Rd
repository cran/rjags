\name{jags.object}
\alias{variable.names.jags}
\alias{coef.jags}
\title{Functions for manipulating jags model objects}
\description{
  A \code{jags} object represents a Bayesian graphical model
  described using the BUGS language. 
}
\usage{
\method{coef}{jags}(object, chain=1, \ldots)
\method{variable.names}{jags}(object, \ldots)
}
\arguments{
  \item{object}{a \code{jags} model object}
  \item{chain}{chain number to query}
  \item{\ldots}{additional arguments to the call (ignored)}
}
\value{
  The \code{coef} function returns a list with an entry for each Node
  array that contains an unobserved Node. Elements corresponding to
  observed Nodes or deterministic Nodes are given missing values.

  The \code{variable.names} function returns a character vector of
  names of node arrays used in the model.
}
\author{Martyn Plummer} \keyword{models}
