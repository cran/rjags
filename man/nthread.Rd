\name{nthread}
\alias{nthread}
\alias{nthread<-}
\title{Get and set number of threads used by a JAGS model}

\usage{
nthread(model)
nthread(model) <- value
}

\arguments{
\item{model}{a \code{jags} model object}
\item{value}{A positive integer}
}

\value{
   A scalar integer, or NULL.
}

\description{
  \code{nthread()} returns the number of parallel threads used by
  a JAGS model.
}

\seealso{
   \code{\link{jags.model}},
}
\keyword{manip}
