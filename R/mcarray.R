#  R package rjags file R/mcarray.R
#  Copyright (C) 2007-2009 Martyn Plummer
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License version
#  2 as published by the Free Software Foundation.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

print.mcarray <- function(x, ...)
{
    if (is.null(dim(x))) {
        NextMethod()
    }
    print(summary(x, mean))
}

summary.mcarray <- function(object, FUN, ...)
{
	
    if (is.null(dim(object))) {
        NextMethod()
    }
	
	# In case it is already pooled over chains and iterations (no dimnames):
	if(length(dim(object))==1 || is.null(names(dim(object))) 
		|| !any(c("iteration","chain") %in% names(dim(object)))){
	    dims <- dim(object)
	    attributes(object) <- NULL
	    dim(object) <- dims
	    ans <- list("stat"=object,
	                "drop.dims" = character(0))
	    class(ans) <- "summary.mcarray"
	    return(ans)
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

make.coda.names <- function(basename, dim)
{
    if (all(dim == 1)) {
        return(basename)
    }
    else {
        ll <- lapply(as.list(dim), function(n) seq(from=1, to=n))
        elements <- expand.grid(ll)
        elt.names <- apply(elements, 1, paste, collapse=",")
        elt.names <- paste0(basename, "[", elt.names, "]")
        return(elt.names)
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
        y <- matrix(aperm(x, perm), nrow=niter, byrow=TRUE)
        ans <- mcmc.list(mcmc(y))
    }
    else {
        nchain <- xdim[which.chain]
        ans <- vector("list",nchain)
        len <- prod(xdim[-which.chain])
        perm <- c((1:ndim)[-c(which.iter,which.chain)], which.iter, which.chain)
        y <- aperm(x,perm)
        for (i in 1:nchain) {
            ans[[i]] <- mcmc(matrix(y[1:len + (i-1)*len], nrow=niter,
                                    byrow=TRUE))
        }
        ans <- mcmc.list(ans)
    }
	
	# If elementnames is set this takes precedence over varname (for eventual use with new monitor types in JAGS 5):
	elt.names <- NULL
	if(!is.null(attr(x, 'elementnames', exact=TRUE))){
		elt.names <- attr(x, 'elementnames')
		if(length(elt.names) != nvar(ans)){
			stop(paste0('The length of the elementnames attr (', length(elt.names), ') does not match the number of variables (', nvar(ans), ')'))
		}
	}
	else if(!is.null(attr(x, "varname", exact=TRUE))){
        elt.names <- coda.names(attr(x, "varname", exact=TRUE),
                               xdim[-c(which.iter, which.chain)])
	}
	
	if(!is.null(elt.names)){
        ### Work around bug in coda::varnames<-
        for (i in 1:nchain) {
            colnames(ans[[i]]) <-elt.names
        }
    }

    return(ans)
}

