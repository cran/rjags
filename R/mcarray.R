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
    if (!checkdimtags(x)) {
        NextMethod()
    }
    else {
        cat("mcarray:",
            sprintf("%s", attr(x, "stat")),
            sprintf("%s", attr(x, "summary")), "\n\n")
        
        print(collapse(x, keep.tags="value", FUN=mean))

        tags <- dimtags(x)
        drop.dims <- tags %in% c("iteration","chain")
        if (any(drop.dims)) {
            cat("\nMarginalizing over:",
                paste(tags[drop.dims], "(", dim(x)[drop.dims],")" ,
                      sep="", collapse=", "),
                "\n")
        }
        
        invisible(x)
    }
}

summary.mcarray <- function(object, FUN, ...)
{
    if (!checkdimtags(object)) {
        NextMethod()
    }
    else {
        collapse(object, keep.tags="value", FUN=FUN, ...)
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

checkdimtags <- function(x)
{
    tags <- dimtags(x)
    if (is.null(tags)) {
        return (FALSE)
    }
    else if (!all(tags %in% c("value", "iteration", "chain"))) {
        return (FALSE)
    }
    return(TRUE)
}

as.mcmc.list.mcarray <- function(x, na.rm=TRUE, ...)
{
    if (is.null(dim(x)) || !checkdimtags(x)) {
        stop("Cannot convert object without dimtags")
    }

    xdim <- dim(x)
    ndim <- length(xdim)
    tags <- dimtags(x)

    mcp <- attr(x, "mcpar")
    if (is.null(mcp)) {
        start <- thin <- 1
    }
    else {
        start <- mcp[1]
        thin <- mcp[3]
    }
    
    which.val <- which(tags == "value")

    which.iter <- which(tags == "iteration")
    if (length(which.iter) == 0) {
        stop("mcarray has no iteration dimension")
    }
    if (length(which.iter) > 1) {
        stop("Multiple iteration dimensions in mcarray")
    }

    which.chain <- which(tags == "chain")
    if (length(which.chain) > 1) {
        stop("Multiple chain dimensions in mcarray")
    }

    niter <- xdim[which.iter]
    if (length(which.chain) == 0) {
        perm <- c(which.val, which.iter)
        y <- matrix(aperm(x, perm), nrow=niter, byrow=TRUE)
        nvar <- ncol(y)
        ans <- list(mcmc(y, start=start, thin=thin))
    }
    else {
        nchain <- xdim[which.chain]
        ans <- vector("list", nchain)
        len <- prod(xdim[-which.chain])
        nvar <- prod(xdim[which.val])
        perm <- c(which.val, which.iter, which.chain)
        y <- aperm(x,perm)

        for (i in 1:nchain) {
            ans[[i]] <- mcmc(matrix(y[1:len + (i-1)*len], nrow=niter, byrow=TRUE),
                             start=start, thin=thin)
        }
    }
    
    val.names <- NULL
    if (!is.null(attr(x, 'valuenames', exact=TRUE))) {
        ## If valuenames attribute is set then use this
        val.names <- attr(x, 'valuenames')
        if (length(val.names) != nvar){
            stop(paste0('The length of the valuenames attr (', length(val.names), ') does not match the number of variables (', nvar, ')'))
        }
    }
    else {
        ## Set default value names based on the varname attribute, if
        ## set. Ignore it it contains square brackets indicating that
        ## it is already a subset.  Fall back to "x" as a generic
        ## variable name
        varname <- attr(x, "varname", exact=TRUE)
        if (is.null(varname) || grepl("\\[", varname)) {
            varname <- "x"
        }
        val.names <-  make.coda.names(varname, xdim[which.val])
    }
    
    if (!is.null(val.names)) {
        for (i in 1:nchain) {
            colnames(ans[[i]]) <- val.names
        }
    }

    if (isTRUE(na.rm)) {
        ## Drop missing values if required
        all.missing <- sapply(ans, function(x) {apply(is.na(x), 2, all)})
        drop.vars <- if (is.matrix(all.missing)) {
                         apply(all.missing, 1, any)
                     }
                     else {
                         any(all.missing)
                     }
        ans <- lapply(ans, function(x) x[, !drop.vars, drop=FALSE])
    }
    
    return(mcmc.list(ans))
}

