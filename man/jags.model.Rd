 \name{jags.model}
 \alias{jags.model}
 \title{Create a JAGS model object}
 \description{
   \code{jags.model} is used to create an object representing a
   Bayesian graphical model, specified with a BUGS-language description
   of the prior distribution, and a set of data.
 }
 \usage{
 jags.model(file, data=sys.frame(sys.parent()), inits, nchain = 1, n.adapt=1000)}
\arguments{
  \item{file}{a file containing a description of the model in the
    JAGS dialect of the BUGS language}
  \item{data}{a list or environment containing the data. Any numeric
    objects in \code{data} corresponding to node arrays used in
    \code{file} are taken to represent the values of observed nodes
    in the model}
  \item{inits}{optional specification of initial values in the form of a
    list or a function (see \code{Initialization} below). If omitted,
    initial values will be generated automatically.  It is an error to
    supply an initial value for an observed node.}
  \item{nchain}{the number of parallel chains for the model}
  \item{n.adapt}{the number of iterations for adaptation.  When a model
    is first created, some of the samplers may have an adaptive mode. In
    this case, the samplers are run for \code{n.adapt} iterations and then
    the adaptive mode is switched off}
}
\value{
  \code{jags.model} returns an object inheriting from class \code{jags}
  which can be used to generate dependent samples from the posterior
  distribution of the parameters
  
  An object of class \code{jags} is a list of function that share a
  common environment. This environment encapsulates the state of the
  model, and the functions can be used to query or modify the model
  state.
  \item{ptr()}{Returns an external pointer to an object created by
  the JAGS library}
  \item{data()}{Returns a list containing the data that defines
  the observed nodes in the model}
  \item{model()}{Returns a character vector containing the BUGS-language
  representation of the model}
  \item{state()}{Returns a list of length equal to the number of
  parallel chains in the model. Each element of the list is itself
  a list containing the current parameter values in that chain.
  This is not the user-level interface: use the \code{\link{coef.jags}}
  method instead.}
  \item{update(niter,by,adapt)}{Updates the model by \code{niter}
  iterations. By default a progress bar is printed to the screen, with
  an update every \code{by} iterations. To suppress the update bar, use 
  \code{by=0}. The \code{adapt} argument is used for adaptive mode.
  This is not the user-level interface: use the \code{\link{update.jags}}
  method instead.}
}
\section{Initialization}{
  There are various ways to specify initial values for a JAGS model.
  If no initial values are supplied, then they will be generated
  automatically by JAGS. See the JAGS User Manual for
  details. Otherwise, the options are as follows:
  \enumerate{
    \item A list of numeric values. Initial values for a single chain
    may supplied as a named list of numeric values. If there are multiple
    parallel chains then the same list is re-used for each chain.
    
    \item A list of lists. Distinct initial values for each chain may be
    given as a list of lists. In this case, the list should have the same
    length as the number of chains in the model.
    
    \item A function.  A function may be supplied that returns a list of
    initial values.  The function is called repeatedly to generate initial
    values for each chain. Normally this function should call some random
    number generating functions so that it returns different values every
    time it is called. The function should either have no arguments, or
    have a single argument named \code{chain}. In the latter case, the
    supplied function is called with the chain number as argument. In this
    way, initial values may be generated that depend systematically on
    the chain number.
  }
}
\author{Martyn Plummer}
\keyword{models}

