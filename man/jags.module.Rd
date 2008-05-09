\name{jags.module}
\alias{jags.module}
\title{Dynamically load JAGS modules}
\description{
  
  A JAGS module is a shared library that extends the functionality of
  JAGS.  This function loads a vector named modules.
}
\usage{
jags.module(names, path)
}
\arguments{
  \item{names}{a vector of names of the jags modules to be loaded}
  \item{path}{the file path to the location of the modules. If omitted,
    the option \code{jags.moddir} is used to locate the modules}
}
\author{Martyn Plummer}
\keyword{interface}

