\name{jags.samples}
\alias{jags.samples}
\title{Generate posterior samples}
\description{
  Function to extract random samples from the posterior distribution
  of the parameters of a \code{jags} model. 
}
\usage{
jags.samples(model, variable.names, n.iter, thin = 1,
             type="trace", ...)
}
\arguments{
  \item{model}{a jags model object}
  \item{variable.names}{a character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{type}{type of monitor}
  \item{...}{optional arguments passed to the update method for jags
             model objects}
}
\details{
  The \code{jags.samples} function creates monitors for the given
  variables, runs the model for \code{n.iter} iterations and returns
  the monitored samples.
}
\value{
  A list of \code{mcarray} objects, with one element for each 
  element of the \code{variable.names} argument.
}
\examples{
  data(LINE)
  LINE$recompile()
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
  n.iter=1000)
  LINE.samples
}
\author{Martyn Plummer}
\seealso{\code{\link{jags.model}}, \code{\link{coda.samples}}}
\keyword{models}
