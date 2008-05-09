"dic.samples" <-
  function(model, n.iter, thin=1, type)
{
    if (nchain(model) == 1) {
        stop("Estimation of the deviance penalty requires 2 or more parallel chains")
    }
    if (!("dic" %in% names(getLoadedDLLs()))) {
        jags.module("dic")
    }
    
    if (class(model) != "jags")
      stop("Invalid JAGS model")
    
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")

    type=match.arg(type, c("pD","popt"))

    .Call("set_default_monitors", model$ptr(), as.integer(thin),
          "deviance", PACKAGE="rjags")
    .Call("set_default_monitors", model$ptr(), as.integer(thin),
          type, PACKAGE="rjags")

    model$update(as.integer(n.iter))
    dev <- .Call("get_monitored_values", model$ptr(), "deviance",
                 PACKAGE="rjags")
    for (i in seq(along=dev)) {
        class(dev[[i]]) <- "mcarray"
    }
    pen <- .Call("get_monitored_values", model$ptr(), type,
                 PACKAGE="rjags")
    for (i in seq(along=pen)) {
        class(pen[[i]]) <- "mcarray"
    }
    
    .Call("clear_default_monitors", model$ptr(), "deviance", PACKAGE="rjags")
    .Call("clear_default_monitors", model$ptr(), type, PACKAGE="rjags")

    ans <-  list(deviance = dev, penalty=pen, type=type)
    class(ans) <- "dic"
    return(ans)
}

"print.dic" <- function(x, digits= max(3, getOption("digits") - 3), ...)
{
    deviance <- sum(sapply(x$deviance, mean))
    cat("Mean deviance: ", format(deviance, digits=digits), "\n")
    N <- length(x$penalty[[1]])
    psum <- rep(0, N)
    for (i in 1:length(x$penalty)) {
        psum <- psum + x$penalty[[i]]
    }
    psum.var <- spectrum0(psum)$spec/N
    cat(x$type, " (Markov Error): ", format(mean(psum), digits=digits),
        " (", format(sqrt(psum.var), digits=digits), ")\n", sep="")
    cat("Penalized deviance:", format(deviance + mean(psum), digits=digits),
        "\n")
    invisible(x)
}

as.mcmc.dic <- function(x)
{
    N <- length(x$deviance)
    Nsample <- length(x$deviance[[1]])

    dn <- names(dim(x$deviance[[1]]))
    which.iter <- which(dn=="iteration")
    
    dev <- pen <- rep(0,Nsample)
    for (i in 1:N) {
        dev <- dev + apply(x$deviance[[i]], which.iter, mean)
        pen <- pen + unclass(x$pen[[i]])
    }

    ans <- matrix(c(dev,pen), nrow=Nsample, ncol=2)
    colnames(ans) <- c("deviance",x$type)
    return(mcmc(ans))
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
