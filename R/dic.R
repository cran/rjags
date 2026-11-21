#  R package rjags file R/dic.R
#  Copyright (C) 2009-2025 Martyn Plummer and Matt Denwood
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

"dic.samples" <- function(model, n.iter, thin=1, type="pD", ...)
{
    if (nchain(model) == 1) {
        stop("2 or more parallel chains required")
    }
    if (!inherits(model, "jags"))
      stop("Invalid JAGS model")
    
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")

    pdtype <- match.arg(type, c("pD","popt"))
    ## Catch bug in JAGS <= 5.0.1 with loo_leverage:
    if (jags.version() <= "5.0.1" && 
        pdtype=="popt") {
      stop("The popt penalty type requires JAGS 5.0.2 or later")
    }    
    penalty_stat <- if (pdtype == "pD") {"leverage"} else {"loo_leverage"}

    samples <- jags.samples(model, variable.names="_observed_",
                            stat=c("deviance", penalty_stat),
                            summary="mean",
                            n.iter=n.iter, thin=thin)
    
    deviance <- collapse(samples[["deviance"]][["mean"]][[1]], keep.tags="value", FUN=mean)
    penalty <- collapse(samples[[penalty_stat]][["mean"]][[1]], keep.tags="value", FUN=mean)
    if (pdtype == "popt") {
        penalty <- 2 * penalty
    }
    
    ans <- list("deviance" = deviance,
                "penalty" = penalty,
                "type" = pdtype)
    class(ans) <- "dic"
    return(ans)
}

"print.dic" <- function(x, digits= max(3, getOption("digits") - 3), ...)
{
    deviance <- sum(x$deviance)
    cat("Mean deviance: ", format(deviance, digits=digits), "\n", sep="")
    psum <- sum(x[[2]])
    cat(names(x)[[2]], ": ", format(mean(psum), digits=digits), "\n", sep="")
    cat("Penalized deviance: ", format(deviance + psum, digits=digits), "\n", sep="")
    invisible(x)
}

"-.dic" <- function(e1, e2)
{
    diffdic(e1, e2)
}
            
"diffdic" <- function(dic1,dic2)
{
    if (!identical(dic1$type, dic2$type)) {
        stop("incompatible dic object: different penalty types")
    }
    n1 <- names(dic1$deviance)
    n2 <- names(dic2$deviance)
    if (!identical(n1, n2)) {

        ### Try matching names in lexicographic order
        if(!identical(sort(n1), sort(n2))) {
            stop("incompatible dic objects: variable names differ")
        }
        ### Reset names to order of the first argument
        ord1 <- order(n1)
        ord2 <- order(n2)
        dic2$deviance[ord1] <- dic2$deviance[ord2]
        dic2$penalty[ord1] <- dic2$penalty[ord2]
    }
    delta <- sapply(dic1$deviance, mean) + sapply(dic1$penalty, mean) -
        sapply(dic2$deviance, mean) - sapply(dic2$penalty, mean)
    class(delta) <- "diffdic"
    return(delta)
}

"print.diffdic" <- function(x, ...)
{
    cat("Difference: ", sum(x), "\n", sep="") 
    cat("Sample standard error: ", sqrt(length(x)) * sd(x), "\n", sep="")
    invisible(x)
}

get_flat_monitors <- function(model, stat, summary)
{
    ## Utility function for waic.samples
    ##
    ## Only for use with monitors that do not have an iteration dimesion
    ## and can therefore be represented as a matrix.
    
    if (!is.character(stat) || length(stat) != 1) stop("Invalid stat")
    if (!is.character(summary) || length(summary) != 1) stop("Invalid summary")
    
    mlist <- .Call("get_monitored_values_flat", model$ptr(), stat, summary, PACKAGE="rjags")
    if (!is.matrix(mlist[[1]])) stop("can only combine matrix-valued mcarrays")
    
    out <- do.call(rbind, mlist)

    ## Set dim tags
    tags <- dimtags(mlist[[1]])
    if (!is.null(tags) && length(tags) == 2) {
        dimtags(out) <- tags
    }
    
    ## Set the value names attribute
    vnames <- unlist(lapply(mlist, function(x) attr(x, "valuenames")))
    if (!is.null(vnames) && length(vnames) == nrow(out)) {
        attr(out, "valuenames") <- vnames
    }
    ## Set the stat and summary attributes
    attr(out, "stat") <- stat
    attr(out, "summary") <- summary
    ## Finally set the class
    class(out) <- "mcarray"

    return(out)
}
    
                              
"waic.samples" <-
    function(model, n.iter, thin=1, nodes="_observed_", scale=2*n, trace=FALSE, ...)
{
    if (!inherits(model, "jags"))
        stop("Invalid JAGS model")
    
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
        stop("n.iter must be a positive integer")

    if (!is.logical(trace) || length(trace)!=1)
        stop("trace must be logical of length 1")

    if (is.null(nodes)) {
        pn <- list(names="_observed_", lower=list(NULL), upper=list(NULL))
    }
    else {
        if (!is.character(nodes) || length(nodes) == 0)
            stop("nodes must either be NULL or a character string of length >=1")
        if (any(nodes == "deviance")) {
            stop("variable name 'deviance' cannot be used: pass nodes='_observed_' for all observed stochastic nodes")
        }
        pn <- parse.varnames(nodes)
    }
    nnames <- length(pn$names)
    
    status1 <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper, 
                     as.integer(thin), rep("likelihood", nnames), rep("mean", nnames), PACKAGE="rjags")
    if (!all(unlist(status1))) stop("Failed to set a necessary monitor")
    
    status2 <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper, 
                     as.integer(thin), rep("loglikelihood", nnames), rep("var", nnames), PACKAGE="rjags")
    if (!all(unlist(status2))) stop("Failed to set a necessary monitor")
    
    if (trace) {
        status3 <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper, 
                         as.integer(thin), rep("loglikelihood", nnames), rep("trace", nnames), PACKAGE="rjags")
        if (!all(unlist(status3))) stop("Failed to set the optional trace monitor")
    }
    
    update(model, n.iter = as.integer(n.iter), ...)

    ## Get monitored values
    likelihood_mean <- get_flat_monitors(model, "likelihood", "mean")
    loglikelihood_var <- get_flat_monitors(model, "loglikelihood", "var")
    if(trace){
        loglikelihood_trace <- get_flat_monitors(model, "loglikelihood", "trace")
    }

    ## Clear monitors
    for(i in seq_along(pn$names)){
        .Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]], pn$upper[[i]], "likelihood", "mean", PACKAGE="rjags")
        .Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]], pn$upper[[i]], "loglikelihood", "var", PACKAGE="rjags")
        if (trace) {
            .Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]], pn$upper[[i]], "loglikelihood", "trace", PACKAGE="rjags")
        }
    }

    ## We assume that monitored nodes are conditionally independent, but it is possible
    ## for the user to specify the same node twice
    if (any(duplicated(rownames(likelihood_mean))))
        warning("Duplicate node names found.")

    
    raw <- list("likelihood_mean"=likelihood_mean, "loglikelihood_var"=loglikelihood_var)
    if (trace) {
        raw <- c(raw, list(loglikelihood_trace = loglikelihood_trace))
    }

    ## Calculate sample size n here for lazy evaluation of the default scale parameter
    n <- nrow(likelihood_mean)
    waictable <- waic.table(likelihood_mean, loglikelihood_var, scale)
    ans <- list(waictable=waictable, mcarray=raw)
    class(ans) <- 'JAGSwaic'	
    
    return(ans)
}


waic.table <- function(likelihood_mean, loglikelihood_var, scale){

    nsample <- nrow(likelihood_mean)
    
    training_loss <- scale * apply(-log(likelihood_mean), 2, sum) / nsample
    p_waic <- apply(loglikelihood_var, 2, sum)
    waic <- training_loss + scale * p_waic / nsample

    result <- rbind(training_loss, p_waic, waic)
    dimnames(result) <- list(c("training_loss", "p_waic", "waic"), paste0('chain', 1:ncol(result)))

    return(result)
}

print.JAGSwaic <- function(x, ...){
    
    print.default(x$waictable, ...)
}
