\name{collapse}
\alias{collapse}
\title{Collapse tagged arrays}
\description{
  Interface to the apply function for tagged arrays
}
\usage{
  collapse(object, drop.tags, keep.tags, FUN, tag.result=FALSE, ...)
}
\arguments{
  \item{object}{An array with a dimtags attribute}
  \item{drop.tags}{tags of dimensions to be dropped}
  \item{keep.tags}{tags of dimensions to be preserved}
  \item{FUN}{the function to be applied}
  \item{tag.result}{binary indicator. If TRUE then the result will also
    be a tagged array if possible}
  \item{...}{additional arguments passed to FUN}
}
\details{
  
  Using the \code{apply} function to simplify high-dimensional arrays
  can be complicated. This function provides a simplified interface for
  tagged arrays with two advantages over the basic \code{apply} function:

  \enumerate{
    \item{Dimensions are referred to by name (dimtags), not by number.}
    \item{The user can define ether the dimensions to be kept or the
      dimensions to be dropped (but not both).}
  }

}
\value{
  Usually the output is a lower-dimensional array (vector, matrix, or
  array). If the output cannot be simplified because \code{FUN} returns
  values of varying length, the output will be a list.

  If \code{tag.result=TRUE}, then the \code{collapse} function will add
  dimtags to the result if it can. This is generally only possible if
  \code{FUN} returns a scalar and the output is not a vector (which has
  no \code{dim} attribute).

  Tagging the result allows a further call to the \code{collapse}
  function. For example, if MCMC output is represented as a tagged
  array, we may wish to calculate the posterior mean within each chain,
  and then the variance of the means between chains.

}
\author{Martyn Plummer}
\seealso{\code{\link{dimtags}}, \code{\link{apply}}}
\keyword{manip}
