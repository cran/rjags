\name{adapt}
\alias{adapt}
\title{Adaptive phase for JAGS models}
\description{
  Update the model in adaptive mode.
}
\usage{
adapt(object, n.iter, end.adaptation=FALSE, \ldots)
}
\arguments{
  \item{object}{a \code{jags} model object}
  \item{n.iter}{length of the adaptive phase}
  \item{end.adaptation}{logical flag. If \code{TRUE} then adaptive
    mode will be turned off on exit.}
  \item{\ldots}{additional arguments to the update method}
}
\value{
  Returns \code{TRUE} if all the samplers in the model have successfully
  adapted their behaviour to optimum performance and \code{FALSE}
  otherwise.
}
\details{

  This function is not normally called by the user.  It is called by the
  \code{jags.model} function when the model object is created.

  When a JAGS model is compiled, it may require an initial sampling
  phase during which the samplers adapt their behaviour to maximize
  their efficiency (e.g. a Metropolis-Hastings random walk algorithm may
  change its step size).  The sequence of samples generated during this
  adaptive phase is not a Markov chain, and therefore may not be used
  for posterior inference on the model.

  The \code{adapt} function updates the model for \code{n.iter}
  iterations in adaptive mode. Then each sampler reports whether it
  has acheived optimal performance (e.g. whether the rejection rate of a
  Metropolis-Hasting sampler is close to the theoretical optimum). If
  any sampler reports failure of this test then \code{adapt} returns
  \code{FALSE}.

  If \code{end.adaptation = TRUE}, then adaptive mode is turned off on
  exit, and further calls to \code{adapt()} do nothing. The model may be
  maintained in adaptive mode with the default option \code{end.adaptation =
  FALSE} so that successive calls to \code{adapt()} may be made until
  adaptation is satisfactory.

}
\author{Martyn Plummer}
\keyword{models}
