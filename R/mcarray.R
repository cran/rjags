print.mcarray <- function(x, ...)
{
    if (is.null(dim(x)) || is.null(names(dim(x)))) {
        NextMethod()
    }
    print(summary(x, mean))
}
   
summary.mcarray <- function(object, FUN, ...)
{
    if (is.null(dim(object)) || is.null(names(dim(object)))) {
        NextMethod()
    }

    dn <- names(dim(object))
    drop.dims <- dn %in% c("iteration","chain")

    ans <- list("stat"=apply(object, which(!drop.dims), FUN, ...),
                "drop.dims" = dim(object)[drop.dims])
    class(ans) <- "summary.mcarray"

    return(ans)
}

print.summary.mcarray <- function(x, ...)
{
    cat("mcarray:\n")
    print(x$stat,...)
    if (length(x$drop.dims) > 0) {
        cat("\nMarginalizing over:", 
            paste(paste(names(x$drop.dims), "(", x$drop.dims,")" , sep=""),
                  collapse=","),"\n")
    }
}

as.mcmc.list.mcarray <- function(x, ...)
{
    if (is.null(dim(x)) || is.null(names(dim(x)))) {
        NextMethod()
    }

    xdim <- dim(x)
    ndim <- length(xdim)
    dn <- names(xdim)

    which.iter <- which(dn=="iteration")
    if (length(which.iter) != 1) {
        stop("Bad iteration dimension in mcarray")
    }
    
    which.chain <- which(dn=="chain")
    if (length(which.chain) > 1) {
        stop("Bad chain dimension in mcarray")
    }

    niter <- xdim[which.iter]
    if (length(which.chain) == 0) {
        perm <- c((1:ndim)[-which.iter], which.iter)
        x <- matrix(aperm(x, perm), nrow=niter, byrow=TRUE)
        ans <- mcmc.list(mcmc(x))
    }
    else {
        nchain <- xdim[which.chain]
        ans <- vector("list",nchain)
        len <- prod(xdim[-which.chain])
        perm <- c((1:ndim)[-c(which.iter,which.chain)], which.iter, which.chain)
        x <- aperm(x,perm)
        for (i in 1:nchain) {
            ans[[i]] <- mcmc(matrix(x[1:len + (i-1)*len], nrow=niter,
                                    byrow=TRUE))
        }
        ans <- mcmc.list(ans)
    }
    return(ans)
}

    
   

