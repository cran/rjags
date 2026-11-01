\name{pkg.compile.flags}
\alias{pkg.compile.flags}
\title{Get package compile flags}
\description{
  Function to check binary compatibility for R packages implementing JAGS
  modules, as well as retrieving the necessary compile flags to link to the
  same version of JAGS that rjags itself uses.
}
\usage{
pkg.compile.flags(type = c("ALL", "JAGS_ROOT", "JAGS_VERSION", "RJAGS_VERSION",
  "CPPFLAGS", "CXXFLAGS", "LIBS"), print=type!="ALL")
}
\arguments{
  \item{type}{the compile flag to display (see value)}
  \item{print}{by default the value is printed using cat (for easier use in
  Makevars files) except when the type is ALL}
}
\details{
  This function is provided for authors of R packages implementing JAGS modules
  for use within the Makevars(.win/ucrt) files in order to ensure that these
  packages are binary compatible with rjags. See also \code{\link{load.pkg.module}}
}
\value{
  The function prints and returns (invisibly) one of the following character strings,
  depending on the type requested:

  \describe{
    \item{ALL}{A list of all the values below (default).}
    \item{JAGS_ROOT}{The location of the JAGS installation that rjags is linked to.}
    \item{JAGS_VERSION}{The version of JAGS linked to.}
    \item{RJAGS_VERSION}{The version of the rjags package.}
    \item{CPPFLAGS}{The required PKG_CPPFLAGS.}
    \item{CXXFLAGS}{The required PKG_CXXFLAGS (may be empty).}
    \item{LIBS}{The required PKG_LIBS.}
  }
}
\examples{
pkg.compile.flags()
}
\author{Matt Denwood}
\keyword{utilities}
