\name{update}
\alias{update.jags}
\title{Update jags models}
\description{
  Update the Markov chain associated with the model. 
}
\usage{
\method{update}{jags}(object, n.iter=1, by, progress.bar, \ldots)
}
\arguments{
  \item{object}{a \code{jags} model object}
  \item{n.iter}{number of iterations of the Markov chain to run}
  \item{by}{refresh frequency for progress bar.  See Details}
  \item{progress.bar}{type of progress bar. Possible values are
    \code{"text"}, \code{"gui"}, and \code{"none"}. See Details.}
  \item{\ldots}{additional arguments to the update method (ignored)}
}
\value{
  The \code{update} method for \code{jags} model objects modifies the
  original object and returns \code{NULL}.
}
\details{

  Since MCMC calculations are typically long, a progress bar is
  displayed during the call to \code{update}.  The type of progress bar
  is determined by the \code{progress.bar} argument.  Type \code{"text"}
  is displayed on the R console. Type \code{"gui"} is a graphical
  progress bar in a new window.  The progress bar is suppressed if
  \code{progress.bar} is \code{"none"} or \code{NULL}, if the update is
  less than 100 iterations, or if R is not running interactively.
  
  The default progress bar type is taken from the option \code{jags.pb}.

  The progress bar is refreshed every \code{by} iterations. The
  update can only be interrupted when the progress bar is refreshed.
  Therefore it is advisable not to set \code{by} to a very large
  value. By default \code{by} is either \code{n.iter/50} or \code{100},
  whichever is smaller.
}
\author{Martyn Plummer}
\keyword{models}
