\name{diffdic}
\alias{diffdic}
\title{Differences in penalized deviance}
\description{
  Compare two models by the difference of two \code{dic} objects.
}
\usage{
\special{dic1 - dic2}
diffdic(dic1, dic2)
} 
\arguments{
  \item{dic1, dic2}{Objects inheriting from class ``dic''}
}
\details{
  A \code{diffdic} object represents the difference in penalized
  deviance between two models. A negative value indicates that
  \code{dic1} is preferred and vice versa.
}
\value{
  An object of class ``diffdic''. This is a numeric vector with
  an element for each observed stochastic node in the model.

  The \code{diffdic} class has its own print method, which will
  display the sum of the differences, and its sample standard
  deviation.
}
\note{
  The problem of determining what is a noteworthy difference
  in DIC (or other penalized deviance) between two models is
  currently unsolved.  Following the results of Ripley (1996) on
  the Akaike Information Criterion, Plummer (2008) argues that 
  there is no absolute scale for comparison of two penalized
  deviance statistics, and proposes that the difference should
  be calibrated with respect to the sample standard deviation
  of the individual contributions from each observed stochastic
  node.
}
\author{Martyn Plummer}
\references{
  Ripley, B. (1996)
  \emph{Statistical Pattern Recognition and Neural Networks.} 
  Cambridge University Press.

  Plummer, M. (2008)
  Penalized loss functions for Bayesian model comparison.
  \emph{Biostatistics}
  doi: 10.1093/biostatistics/kxm049
}
\seealso{\code{\link{dic}}}
\keyword{models}
