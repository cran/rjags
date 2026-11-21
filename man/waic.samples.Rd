\name{waic.samples}
\alias{waic}
\alias{waic.samples}
\title{Generate samples for the Widely Applicable Information Criterion}
\description{
  Function to extract random samples of the Widely Applicable
  Information  Criterion (WAIC) from  a \code{jags} model.
}
\usage{
waic.samples(model, n.iter, thin = 1, nodes="_observed_", scale=2*n, trace=FALSE, ...)
}
\arguments{
  \item{model}{A jags model object.}
  \item{n.iter}{Number of iterations to monitor.}
  \item{thin}{Thinning interval for monitors.}
  \item{nodes}{character vector of variable names containing nodes to be
    monitored. A variable name can be the name of a whole node array,
    e.g. "x", or subset, e.g. "x[2:5,2]". The monitored nodes will be any
    nodes partially or completely contained in the given variable array or
    subset. The default value "_observed_" monitors all observed stochastic nodes.}
  \item{scale}{Scaling factor for WAIC. The default scaling by twice the
    sample size \code{n} gives a WAIC statistic comparable with AIC and DIC.}
  \item{trace}{Logical flag. If TRUE then a trace of the likelihood
    will be returned for each monitored node in addition to the WAIC
    table.}
  \item{...}{optional arguments passed to the update method for jags
             model objects}
}
\value{
  An object of class "JAGSwaic". This is a list containing the following
  elements:
  \item{waic.table}{A matrix with one column for each chain and 3 rows
    \describe{
      \item{training_loss}{The Bayes training loss multiplied by the
	\code{scale} parameter.}
      \item{p_waic}{The WAIC penalty, measuring the dimension of the model}.
      \item{waic}{Penalized version of the Bayes training loss estimating
	the Bayes generalization error.}
    }
  }
  \item{mcarray}{A list of objects of class "mcarray" containing at
    least the following elements
    \describe{
      \item{likelihood_mean}{The posterior mean of the likelihood for each
	node. This is the posterior predictive density evaluated at the
	observed value.}
      \item{loglikelihood_var}{The posterior variance of the likelihood
	for each node. This is the contribution of each node to the WAIC
	penalty \code{p_waic} and can also be considered a measure of
	local influence for each observation (Plummer 2025).}
    }
    Optionally, if \code{trace=TRUE} the list includes
    \describe{
      \item{loglikelihood_trace}{Trace of the log likelihood for each
	monitored node. If there are a large number of nodes this may
	use a lot of memory.}
    }
  }
}
\details{

  The \code{waic.samples} function calculates the Widely Applicable
  Information Criterion (WAIC) (Watanabe 2010), a predictive information
  criterion for model choice based on singular learning theory (Watanabe
  2009).

  Suppose that \eqn{Y_1 \ldots Y_n}{Y[1] ... Y[n]} are exchangeable
  observations that are conditionally independent given parameters
  \eqn{\theta}{theta}. The posterior predictive density \eqn{q(y)}{q(y)}
  is defined as:

  \deqn{
    q(y) = \text{E}_{\theta} \left[p(y \mid \theta) \mid Y \right]
  }{
    q(y) = E [ p(y | theta) | Y ]
  }

  The princple behind WAIC is that model fit should be measured by
  the Bayes generalization error \eqn{BL_g}{BLg}, which is minus the
  expectation of the log predictive density for a new observation
  \eqn{Y_{n+1}}{Y[n+1]}.

  \deqn{
    BL_g = \text{E} \left[ \log q(Y_{n+1}) \right]
  }{
    BLg = - E [ log q(Y[n+1]) ]
  }

  where the expectation is over the true, unknown distribution of
  \eqn{Y_{n+1}}{Y[n+1]}. In practice, this cannot be evaluated but we can
  approximate the Bayes generalization error by replacing the
  expectation with the empirical average over the observed data. This
  gives the Bayes training loss.

  \deqn{
    BL_t = - \frac{1}{n} \sum_{i=1}^n \log \left[ q(Y_i) \right]
  }{
    BLt = - mean( log [q(Y[1:n])] )
  }
  
  The training loss is a biased estimate of the generalization error. It
  systematically under-estimates the generalization error because it
  uses the data twice: once to evaluate the posterior predictive
  distribution and again to evaluate the expectation over the data.

  Watanabe (2009, 2010) showed that the bias of the Bayes training loss
  can be removed asymptotically for large sample size n by adding a
  penalty \eqn{p_W/n}{pW/n} to give the WAIC:

  \deqn{
    \text{WAIC} = BL_t + \frac{p_W}{n}
  }{
    WAIC = BLt + pW/n
  }

  where

  \deqn{
    p_W = \sum_{i=1}^n \text{Var}_{\theta} \left[ \log p\left(Y_i \mid
    \theta \right) \mid Y \right]
  }{
    pW = sum(var [log p(y | theta) | Y])
  }

  The penalty \eqn{p_W}{pW} can be considered a measure of model
  complexity.  In regular models with non-informative priors,
  \eqn{p_W}{pW} reduces to the dimension of the parameters
  \eqn{\theta}{theta}.
  
  It is common practice to use version of WAIC scaled by
  \eqn{2n}{2*n}. This version of WAIC can be written:
  
  \deqn{
    WAIC = - 2 \sum_{i=1}^n \log \left[ q(Y_i) \right] + 2 p_W
  }{
    WAIC = - 2 * sum( log [q(Y[1:n])] ) + 2 * pW
  }
  
  In this form, WAIC is comparable to the Akaike Information Criterion
  (AIC) and the Deviance Information Criterion (Spiegelhalter et al
  2002). Scaling of WAIC is controlled by the \code{scale} parameter.
  
}
\author{Matt Denwood, Martyn Plummer}
\references{
  Plummer, M. (2025) Bayesian measures of leverage and influence.
  \emph{Arxiv}, \url{https://arxiv.org/html/2503.19996}
  
  Spiegelhalter, D., N. Best, B. Carlin, and A. van der Linde (2002),
  Bayesian measures of model complexity and fit (with discussion).
  \emph{Journal of the Royal Statistical Society Series B}
  \bold{64}, 583-639.

  Watanabe, S (2009), Algebraic Geometry and Statistical Learning
  Theory, Cambridge University Press.

  Watanabe, S (2010) Asymptotic Equivalence of Bayes Cross Validation
  and Widely Applicable Information Criterion in Singular Learning
  Theory, Journal of Machine Learning Research, 11, 116, 3571--3594.
}
\seealso{\code{\link{dic.samples}}}
\keyword{models}
