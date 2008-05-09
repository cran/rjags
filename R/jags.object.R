update.jags <- function(object, niter = 1, ...)
{
    if (!is.numeric(niter) || niter < 1) {
        stop("Invalid niter")
    }
    object$update(niter)
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
