update.jags <- function(object, n.iter = 1, by, ...)
{
    if (!is.numeric(n.iter) || n.iter < 1) {
        stop("Invalid n.iter")
    }
    if (missing(by))
	by <- floor(n.iter/50)

    object$update(n.iter, by)
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
