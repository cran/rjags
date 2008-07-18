update.jags <- function(object, niter = 1, by, ...)
{
    if (!is.numeric(niter) || niter < 1) {
        stop("Invalid niter")
    }
    if (missing(by))
	by <- floor(niter/50)

    object$update(niter, by)
    invisible(NULL)
}
    
coef.jags <- function(object, chain = 1, ...) {
    if (!is.numeric(chain) || chain < 1 || chain > object$nchain()) {
        stop("Invalid chain")
    }
    object$state(internal=FALSE)[[chain]]
}

variable.names.jags <- function(object, ...) {
    .Call("get_variable_names", object$ptr(), PACKAGE="rjags")
}
