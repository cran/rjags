\name{jags.module}
\alias{load.module}
\alias{unload.module}
\alias{list.modules}
\title{Dynamically load JAGS modules}
\description{
  A JAGS module is a dynamically loaded library that extends the
  functionality of JAGS. These functions load and unload JAGS modules
  and show the names of the currently loaded modules.
}
\usage{
load.module(name, path, quiet=FALSE)
unload.module(name, quiet=FALSE)
list.modules()
}
\arguments{
  \item{name}{name of the load module to be loaded}
  \item{path}{file path to the location of the DLL. If omitted,
    the option \code{jags.moddir} is used to locate the modules}
  \item{quiet}{a logical. If \code{TRUE}, no message will be printed
    about loading the package}
}
\examples{
list.modules()
load.module("glm")
list.modules()
unload.module("glm")
list.modules()
}
\author{Martyn Plummer}
\keyword{interface}

