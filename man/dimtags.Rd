\name{dimtags}
\alias{dimtags}
\alias{dimtags<-}
\title{Get and set dimtags for a tagged array}

\usage{
dimtags(x)
dimtags(x) <- value
}

\arguments{
  \item{x}{A matrix, array, or any object with a  \code{dim} attribute}
  \item{value}{A character vector of tags, which must match the number
    of dimensions}
}

\value{

  A character vector of dimtags. The replacement function returns the
  modified array.

}

\description{

  Dimtags are designed to make working with high-dimensional arrays
  simpler by providing names for each dimension. We use the word "tags"
  instead of names for two reasons
  \enumerate{
    \item{To avoid confusion with dimnames which already exists and
      means something different}
    \item{Because more than dimension can have the same tag. So tags
      do not uniquely identify each dimension}
  }

  This infrastructure was put in place to support objects of class
  \code{mcarray}, which are tagged arrays with some additional
  attributes. The primary use of tagging is to simplify the manipulation
  of high-dimensional arrays using the \code{collapse} function instead
  of \code{apply}.
  
}

\seealso{
   \code{\link{collapse}}, \code{\link{mcarray.object}},
}
\keyword{manip}
