\name{rjags-package}
\alias{rjags-package}
\alias{rjags}
\docType{package}
\title{
Bayesian graphical models using MCMC
}
\description{

  The rjags package provides an interface from R to the JAGS library for
  Bayesian data analysis.  JAGS uses Markov Chain Monte Carlo (MCMC) to
  generate a sequence of dependent samples from the posterior
  distribution of the parameters.

}
\details{

  JAGS is a clone of BUGS (Bayesian analysis Using Gibbs Sampling).  See
  Lunn et al (2009) for a history of the BUGS project.  Note that the
  rjags package does not include a copy of the JAGS library: you must
  install this separately. For instructions on downloading JAGS, see the
  home page at \url{http://mcmc-jags.sourceforge.net}.
  
  To fully understand how JAGS works, you need to read the
  \href{http://sourceforge.net/projects/mcmc-jags/files/Manuals/3.x/jags_user_manual.pdf}{JAGS
    User Manual}.  The manual explains the basics of modelling with JAGS
  and shows the functions and distributions available in the dialect of
  the BUGS language used by JAGS. It also describes the command line
  interface.  The \pkg{rjags} package does not use the command line
  interface but provides equivalent functionality using R functions.
  
  Analysis using the \pkg{rjags} package proceeds in steps:
  \enumerate{

    \item Define the model using the BUGS language in a separate file.

    \item Read in the model file using the \code{jags.model} function.
    This creates an object of class \dQuote{jags}.

    \item Update the model using the \code{update} method for
    \dQuote{jags} objects.  This constitutes a \sQuote{burn-in} period.

    \item Extract samples from the model object using the
    \code{coda.samples} function. This creates an object of class
    \dQuote{mcmc.list} which can be used to summarize the posterior
    distribution. The \pkg{coda} package also provides convergence
    diagnostics to check that the output is valid for analysis (see
    Plummer et al 2006).

  }
}
\author{
Martyn Plummer
}
\references{
Lunn D, Spiegelhalter D, Thomas A, Best N. (2009) The BUGS project:
Evolution, critique and future directions.  \emph{Statistics in Medicine}, 
\bold{28}:3049-67.

Plummer M, Best N, Cowles K, Vines K (2006). CODA: Convergence Diagnosis
and Output Analysis for MCMC, \emph{R News}, \bold{6}:7-11.

}
\keyword{ package }
%\examples{
%
%}
