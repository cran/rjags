\name{jags.version}
\alias{jags.version}
\title{Get JAGS version}
\description{
  Function to find the version of the \code{JAGS} library linked to the
  \code{rjags} package.
}
\usage{
jags.version()
}
\details{
  The \code{rjags} package links to the \code{JAGS} library when loaded.
  This function returns the version of the linked \code{JAGS} library.

  In principle, the major version of the \code{rjags} package should
  match major version of \code{JAGS}.  The \code{jags.version()}
  function also returns information on the minor version and release
  number of \code{JAGS}.
}
\value{
  An object of class \code{"numeric_version"}.
}
\examples{
  jags.version()
}
\author{Matt Denwood}
\seealso{numeric_version}
\keyword{utilities}
