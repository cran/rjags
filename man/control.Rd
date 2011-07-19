\name{control}
\alias{list.factories}
\alias{set.factory}
\title{Advanced control over JAGS}
\description{
  JAGS modules contain factory objects for samplers, monitors, and
  random number generators for a JAGS model.  These functions allow
  fine-grained control over which factories are active.
}
\usage{
list.factories(type)
set.factory(name, type, state)
}
\arguments{
  \item{name}{name of the factory to set}
  \item{type}{type of factory to query or set. Possible values are
    \code{"sampler"}, \code{"monitor"}, or \code{"rng"}}
  \item{state}{a logical. If \code{TRUE} then the factory will be
    active, otherwise the factory will become inactive.}
}
\value{
  \code{list.factories} returns a data frame with two columns, the first
  column shows the names of the factory objects in the currently loaded
  modules, and the second column is a logical vector indicating whether
  the corresponding factory is active or not.

  \code{set.factory} is called to change the future behaviour of factory
  objects. If a factory is set to inactive then it will be skipped.
} 
\note{
  When a module is loaded, all of its factory objects are active. This
  is also true if a module is unloaded and then reloaded.
}
\examples{
list.factories("sampler")
list.factories("monitor")
list.factories("rng")
set.factory("base::Slice", "sampler", FALSE)
list.factories("sampler")
set.factory("base::Slice", "sampler", TRUE)
}
\author{Martyn Plummer} \keyword{models}
