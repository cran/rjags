\name{read.data}
\alias{read.data}
\title{Read data files for jags models}
\description{
  Read data for a JAGS model from a file.
}
\usage{
read.data(file, format=c("jags","bugs"))
}
\arguments{
  \item{file}{name of a file containing a text repesentation of the
    data for a jags model}
  \item{format}{format of the data. See Details below}
}
\value{
  A named list of numeric vectors or arrays. 
}
\details{
  
  The command line interface for JAGS reads data and initial values from
  a text file.  The data format used for jags data files is the same as the R
  \code{dump} function. Thus the data values can be read into an
  R session using the \code{source} function, but this will create
  objects in the global environment. The \code{read.data} function,
  with \code{format="jags"}, is a simple wrapper that reads the data
  into a list instead.

  OpenBUGS also reads data and initial values from a text file. The
  format of these files is described as "S-PLUS" format by the
  OpenBUGS authors. It superficially resembles the format used by the
  \code{dput} function (and in fact can be parsed by the \code{dget}
  function). However, in BUGS "S-PLUS" format, arrays are stored
  in row-major order instead of the column-major order used by R.
  The \code{read.data} function, with \code{format="bugs"} reads OpenBUGS
  "S-PLUS" format files and permutes the elements of arrays so that they
  appear in the correct order.

  Either choice of format returns a list which can be used as the
  \code{data} or \code{inits} argument of \code{jags.model}.
} 
\author{Martyn Plummer} \keyword{file}
