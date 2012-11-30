"dic.samples" <-
  function(model, n.iter, thin=1, type="pD", ...)
{
    if (nchain(model) == 1) {
        stop("2 or more parallel chains required")
    }
    if (!inherits(model, "jags"))
      stop("Invalid JAGS model")
    
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")
    load.module("dic", quiet=TRUE)
    limits <- vector("list",2)
    pdtype <- match.arg(type, c("pD","popt"))
    status <- .Call("set_monitors", model$ptr(), c("deviance",pdtype),
                    limits, limits, as.integer(thin), "mean", PACKAGE="rjags")
    if (!any(status)) {
      stop("Failed to set monitors")
    }
    
    update(model, n.iter = as.integer(n.iter), ...)
    dev <- .Call("get_monitored_values_flat", model$ptr(), "mean",
                 PACKAGE="rjags")
    for (i in seq(along=dev)) {
        class(dev[[i]]) <- "mcarray"
    }

    if (status[1]) {
        .Call("clear_monitor", model$ptr(), "deviance", NULL, NULL, "mean",
              PACKAGE="rjags")
    }
    if (status[2]) {
        .Call("clear_monitor", model$ptr(), pdtype, NULL, NULL, "mean",
              PACKAGE="rjags")
    }

    ans <- list("deviance" = dev$deviance, "penalty" = dev[[type]],
                "type" = type)
    class(ans) <- "dic"
    return(ans)
}

"print.dic" <- function(x, digits= max(3, getOption("digits") - 3), ...)
{
    deviance <- sum(x$deviance)
    cat("Mean deviance: ", format(deviance, digits=digits), "\n")
    psum <- sum(x[[2]])
    cat(names(x)[[2]], format(mean(psum), digits=digits), "\n")
    cat("Penalized deviance:", format(deviance + psum, digits=digits), "\n")
    invisible(x)
}

"-.dic" <- function(e1, e2)
{
    diffdic(e1, e2)
}
            
"diffdic" <- function(dic1,dic2)
{
    if(!identical(names(dic1$deviance),names(dic2$deviance))) {
        stop("incompatible dic objects: variable names differ")
    }
    if (!identical(dic1$type, dic2$type)) {
        stop("incompatible dic object: different penalty types")
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
