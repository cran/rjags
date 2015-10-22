\name{coda.samples}
\alias{coda.samples}
\title{Generate posterior samples in mcmc.list format}
\description{
  This is a wrapper function for \code{jags.samples} which sets a trace
  monitor for all requested nodes, updates the model, and coerces the
  output to a single \code{mcmc.list} object.
}
\usage{
coda.samples(model, variable.names, n.iter, thin = 1, na.rm=TRUE, ...)
}
\arguments{
  \item{model}{a jags model object}
  \item{variable.names}{a character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{na.rm}{logical flag that indicates whether variables
    containing missing values should be omitted. See details.}
  \item{...}{optional arguments that are passed to the update method
             for jags model objects}
}
\value{
  An \code{mcmc.list} object.
}
\details{
  If \code{na.rm=TRUE} (the default) then elements of a variable that
  are missing (\code{NA}) for any iteration in at least one chain will
  be dropped.

  This argument was added to handle incompletely defined variables.
  From JAGS version 4.0.0, users may monitor variables that are not
  completely defined in the BUGS language description of the model,
  e.g. if \code{y[i]} is defined in a \code{for} loop starting from
  \code{i=3} then \code{y[1], y[2]} are not defined. The user may still
  monitor variable \code{y} and the monitored values corresponding to
  \code{y[1], y[2]} will have value \code{NA} for all iterations in all
  chains. Most of the functions in the \pkg{coda} package cannot handle
  missing values so these variables are dropped by default.
}
\examples{
data(LINE)
LINE$recompile()
LINE.out <- coda.samples(LINE, c("alpha","beta","sigma"), n.iter=1000)
summary(LINE.out)
}
\author{Martyn Plummer}
\seealso{\code{\link{jags.samples}}}
\keyword{models}
