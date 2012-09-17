\name{rjags-deprecated}
\alias{rjags-deprecated}
\alias{read.data}
\title{Deprecated Functions in the rjags package}
\description{
  These functions are provided for compatibility with older versions of
  the rjags package and will soon be defunct.
}
\usage{
read.data(file, format=c("jags","bugs"))
}
\arguments{
  \item{file}{name of a file containing a text repesentation of the
    data for a jags model}
  \item{format}{format of the data}
}
\details{
  
  \code{read.data} with \code{format="jags"} is a deprecated synonym for
  \code{\link{read.jagsdata}} and with
  \code{format="bugs"} is a deprecated synonym for
  \code{\link{read.bugsdata}}.

}
\keyword{misc}
