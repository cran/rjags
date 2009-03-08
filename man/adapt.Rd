\name{update}
\alias{adapt}
\title{Adaptive phase for JAGS models}
\description{
  
  A \code{jags} object represents a Bayesian graphical model
  described using the BUGS language. 
}
\usage{
adapt(object, n.iter, \dots)
}
\arguments{
  \item{object}{a \code{jags} model object}
  \item{n.iter}{length of the adaptive phase}
  \item{\ldots}{additional arguments to the update method}
}
\value{
  This function modifies the original object and returns \code{NULL}
}
\details{
  This function is not normally called by the user.

  When a JAGS model is compiled, it may require an initial sampling phase
  during which the samplers adapt their parameters to maximize their
  efficiency. The sequence of samples generated during this
   adaptive phase is not a Markov chain, and therefore may not be used
  for posterior inference on the model. For this reason, the \code{adapt}
  function must be called before samples can be generated from the
  model using the \code{update} method.  Normally, this is done by the
  \code{jags.model} function when the model object is created.

  The \code{adapt} function can only be called once on a jags model object
  since it turns off the adaptive phase. Subsequent calls to \code{adapt}
  do nothing.
}
\author{Martyn Plummer} \keyword{models}
