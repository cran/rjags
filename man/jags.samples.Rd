\name{jags.samples}
\alias{jags.samples}
\title{Generate posterior samples}
\description{
  Function to extract random samples from the posterior distribution
  of the parameters of a \code{jags} model. 
}
\usage{
jags.samples(model, variable.names, n.iter, thin = 1,
             stat="value", summary = "trace", simplify=2, ...)
}
\arguments{

  \item{model}{A jags model object}
  \item{variable.names}{A character vector giving the names of variables
    to be monitored}
  \item{n.iter}{number of iterations to monitor}
  \item{thin}{thinning interval for monitors}
  \item{stat}{character vector of statistics to be monitored}
  \item{summary}{character vector of summaries}
  \item{simplify}{scalar integer or logical flag that determines whether
    the return value will be simplified. See Value below.}
  \item{...}{optional arguments passed to the update method for jags
    model objects}

}
\details{

  The \code{jags.samples} function creates monitors for the given
  variables, runs the model for \code{n.iter} iterations and returns
  the monitored samples.

}
\value{

  A nested list of \code{mcarray} objects is returned.  The top level
  denotes the requested stat, the second level the summary, and the
  third level the monitored variable. Individual \code{mcarray} objects
  can be extrected with \code{samples$stat$summary$variable}
  substituting appropriate names for \code{stat}, \code{summary}, and
  \code{variable}, e.g. \code{samples$value$trace$alpha}.

  In mathematical terms, the return value is a tree with \code{mcarray}
  objects as leaf nodes. If the root node of the tree is unary, i.e. it
  has only one child node, then we can simplify the return value by
  returning the next level of the tree. This is controlled by the
  \code{simplify} parameter. If \code{simplify} is an integer between 1
  and 3 then it determines the maximum depth of the simplification. A
  value of 0 indicates no simplification. For backward compatibility
  with JAGS 4, the default value is \code{simplify=2} meaning that
  levels corresponding to \code{stat} and \code{summary} will be
  simplified, if they are unary, but not \code{value}.

  The \code{simplify} parameter may also be a logical flag where
  \code{FALSE} means no simplification and \code{TRUE} is equivalent to
  \code{simplify=3}.  This is most useful when only a single
  stat/summary/variable combination is monitored, in which case a single
  \code{mcarray} object is returned instead of a nested list.

}
\examples{
  data(LINE)
  LINE$recompile()
  ## With simplify=FALSE the output is a nested list of depth 3
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
                               n.iter=1000, simplify=FALSE)
  ## Access the value trace monitor for node alpha
  LINE.samples$value$trace$alpha

  ## With simplify=2, the top 2 levels of the list are removed
  ## because all monitors are of stat type "value" and summary type "trace"
  LINE.samples <- jags.samples(LINE, c("alpha","beta","sigma"),
                               n.iter=1000, simplify=2)
  ## Simpler access to the value trace monitor for alpha
  LINE.samples$alpha

  ## If we have monitors of the same stat type but different summary
  ## types then simplify will only remove the top level of the list,
  ## even if we request maximum simplification
  LINE.samples <- jags.samples(LINE, "alpha",  n.iter=1000,
                               summary=c("mean","var"),
                               simplify=3)
  LINE.samples$mean$alpha
}
\author{Martyn Plummer}
\seealso{\code{\link{jags.model}}, \code{\link{coda.samples}}}
\keyword{models}
