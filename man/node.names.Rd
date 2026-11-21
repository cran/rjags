\name{node.names}
\alias{node.names}
\alias{observed.stochastic.nodes}
\alias{expand.name}
\title{Utilities for extracting node names from JAGS models}
\description{
  These utility functions allow the user to obtain the names of nodes within
  a compiled JAGS model. The \code{node.names} function returns the names of all nodes
  within the graph (without array indices), and the \code{observed.stochastic.nodes}
  function returns only the node names and incidies that represent observed
  stochastic variables. The \code{expand.name} function converts a base array name
  (as returned by \code{node.names}) along with a provided vector of array dimensions
  into the vector of node names as returned by \code{observed.stochastic.nodes}
  and the \code{\link{coda.samples}} function.
}
\usage{
node.names(model)
observed.stochastic.nodes(model)
expand.name(name, dimensions)
}
\arguments{
  \item{model}{a jags model object}
  \item{name}{a length-1 character string giving the variable name to expand}
  \item{dimensions}{an integer vector giving the dimensions of the variable}
}
\examples{
data(LINE)
LINE$recompile()
node.names(LINE)
observed.stochastic.nodes(LINE)
expand.name("Y", 5)
}
\author{Matt Denwood}
\keyword{interface}

