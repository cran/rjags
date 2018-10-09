\name{jags.samples}
\alias{jags.samples}
\title{Generate posterior samples}
\description{
  Function to extract random samples from the posterior distribution
  of the parameters of a \code{jags} model. 
}
\usage{
jags.samples(model, variable.names, n.iter, thin = 1,
             type="trace", force.list=FALSE, ...)
}
\arguments{
  \item{model}{a jags model object}
  \item{variable.names}{a character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{type}{type of monitor (can be vectorised)}
  \item{force.list}{option to consistently return a named list of monitor
             types even if a single monitor type is requested}
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
  element of the \code{variable.names} argument.  If more than 
  one type of monitor is requested (or if force.list is TRUE)
  then the return value will be a (named) list of lists of 
  \code{mcarray} objects, with one element for each monitor type.
}
\examples{
  data(LINE)
  LINE$recompile()
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
  n.iter=1000)
  LINE.samples
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
  force.list=TRUE, n.iter=1000)
  LINE.samples
  LINE.samples <- jags.samples(LINE, c("alpha","alpha"),
  n.iter=1000, type=c("trace","mean"))
  LINE.samples$trace
  LINE.samples$mean
}
\author{Martyn Plummer}
\seealso{\code{\link{jags.model}}, \code{\link{coda.samples}}}
\keyword{models}
