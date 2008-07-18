print.jags <- function(x, ...)
{
  cat("JAGS model:\n\n")
  
  model <- x$model()
  for (i in 1:length(model)) {
    cat(model[i],"\n",sep="")
  }

  data <- x$data()
  full <- !sapply(lapply(data, is.na), any)
  if (any(full)) {
    cat("Fully observed variables:\n", names(data)[full], "\n")
  }
  part <- !full & !sapply(lapply(data, is.na), all)
  if (any(part)) {
    cat("Partially observed variables:\n", names(data)[part], "\n")
  }
}

jags.model <- function(file, data=sys.frame(sys.parent()), inits,
                       nchain = 1, n.adapt=1000)
{

    if (missing(file)) {
        stop("Model file name missing")
    }
    p <- .Call("make_console", PACKAGE="rjags") 
    .Call("check_model", p, file, PACKAGE="rjags")

    varnames <- .Call("get_variable_names", p, PACKAGE="rjags")
    if (is.environment(data)) {
        ##Get a list of numeric objects from the supplied environment
        data <- mget(varnames, envir=data, mode="numeric",
                     ifnotfound=list(NULL))
        ##Strip null entries
        data <- data[!sapply(data, is.null)]
    }
    else if (is.list(data)) {
        v <- names(data)
        if (is.null(v)) {
            stop("data must be a named list")
        }
        if (any(nchar(v)==0)) {
            stop("unnamed variables in data list")
        }
        if (any(duplicated(v))) {
            stop("Duplicated names in data list: ",
                 paste(v[duplicated(v)], collapse=" "))
        }
        relevant.variables <- names(data) %in% varnames
        data <- data[relevant.variables]
    }
    else {
        stop("data must be a list or environment")
    }
    
    .Call("compile", p, data, as.integer(nchain), TRUE, PACKAGE="rjags")

### Setting initial values

    if (!missing(inits)) {

        checkParameters <- function(inits) {
            if(!is.list(inits))
              return (FALSE)
            if (is.null(names(inits)) || any(nchar(names(inits)) == 0))
              return (FALSE)
            if (!all(sapply(inits, is.numeric)))
              return (FALSE)
            
            return (TRUE)
        }
        
        setParameters <- function(inits, chain) {
            if (!is.null(inits[[".RNG.name"]])) {
                .Call("set_rng_name", p, inits[[".RNG.name"]], PACKAGE="rjags")
                inits[[".RNG.name"]] <- NULL
            }
            .Call("set_parameters", p, inits, as.integer(chain),
                  PACKAGE="rjags")
        }
        
        init.values <- vector("list", nchain)
        
        if (is.function(inits)) {
            if (any(names(formals(inits)) == "chain")) {
                for (i in 1:nchain) {
                    init.values[[i]] <- inits(chain=i)
                }
            }
            else {
                for (i in 1:nchain) {
                    init.values[[i]] <- inits()
                }
            }
        }
        else if (is.list(inits)) {

            if (checkParameters(inits)) {
                ## Replicate initial values for all chains
                for (i in 1:nchain) {
                    init.values[[i]] <- inits
                }
            }
            else {
                if (length(inits) != nchain) {
                    stop("Length mismatch in inits")
                }
                init.values <- inits
            }
        }
            
        for (i in 1:nchain) {
            if (!checkParameters(init.values[[i]])) {
                stop("Invalid parameters for chain ", i)
            }
            setParameters(init.values[[i]], i)
        }
    }

    .Call("initialize", p, PACKAGE="rjags")

    model.state <- .Call("get_state", p, PACKAGE="rjags")
    model.data <- .Call("get_data", p, PACKAGE="rjags")
    model.code <- readLines(file)
    model <- list("ptr" = function() {p},
                  "data" = function() {model.data},
                  "model" = function() {model.code},
                  "state" = function(internal=FALSE)
                  {
                      if(!internal) {
                          for(i in 1:nchain) {
                              model.state[[i]][[".RNG.state"]] <- NULL
                              model.state[[i]][[".RNG.name"]] <- NULL
                          }
                      }
                      return(model.state)
                  },
                  "nchain" = function()
                  {
                      .Call("get_nchain", p, PACKAGE="rjags")
                  },
                  "iter" = function()
                  {
                      .Call("get_iter", p, PACKAGE="rjags")
                  },
                  "update" = function(niter, by=niter/50, adapt=FALSE) {

                    adapting <- .Call("is_adapting", p, PACKAGE="rjags")
                    if (adapt & !adapting)
                      return(invisible(NULL))

                    if (niter <= 0)
                      stop("niter must be positive")
                    niter <- floor(niter)

                    if (by <= 0)
                      stop("by must be positive")
                    by <- ceiling(by)

                    if (interactive()) {
                      #Show progress bar
                      pb <- txtProgressBar(0, niter, style=1,width=50,
                                           char=ifelse(adapting,"+","*"))
                      n <- niter
                      while (n > 0) {
                        .Call("update", p, min(n,by), adapt, PACKAGE="rjags")
                        n <- n - by
                        setTxtProgressBar(pb, niter - n)
                        model.state <<- .Call("get_state", p, PACKAGE="rjags")
                      }
                      close(pb)
                    }
                    else {
                      #Suppress progress bar
                      .Call("update", p, niter, adapt, PACKAGE="rjags")
                      model.state <<- .Call("get_state", p, PACKAGE="rjags")
                    }
                    
                    if (adapting) {
                      if (!.Call("adapt_off", p, PACKAGE="rjags")) {
                        warning("Adaptation incomplete");
                      }
                    }
                    
                    invisible(NULL)
                  },
                  "recompile" = function() {
                      ## Clear the console
                      .Call("clear_console", p, PACKAGE="rjags")
                      p <<- .Call("make_console", PACKAGE="rjags")
                      ## Write the model to a temporary file so we can re-read it
                      mf <- tempfile()
                      writeLines(model.code, mf)
                      .Call("check_model", p, mf, PACKAGE="rjags")
                      unlink(mf)
                      ## Re-compile
                      .Call("compile", p, data, nchain, FALSE, PACKAGE="rjags")
                      ## Re-initialize
                      if (!is.null(model.state)) {
                          if (length(model.state) != nchain) {
                              stop("Incorrect number of chains in saved state")
                          }
                          for (i in 1:nchain) {
                              statei <- model.state[[i]]
                              rng <- statei[[".RNG.name"]]
                              if (!is.null(rng)) {
                                  .Call("set_rng_name", p, rng, i, PACKAGE="rjags")
                                  statei[[".RNG.name"]] <- NULL
                              }
                              .Call("set_parameters", p, statei, i, PACKAGE="rjags")
                          }
                          .Call("initialize", p, PACKAGE="rjags")
                          #Redo adaptation
                          cat("Adapting\n")
                          .Call("update", p, n.adapt, TRUE, PACKAGE="rjags")
                          model.state <<- .Call("get_state", p, PACKAGE="rjags")
                      }
                      invisible(NULL)
                  })
    class(model) <- "jags"
    model$update(n.adapt, adapt=TRUE)
    return(model)
}

jags.samples <-
  function(model, variable.names=NULL, n.iter, thin=1, type="trace")
{
    if (class(model) != "jags")
      stop("Invalid JAGS model")

    if (!is.null(variable.names)) {
       if (!is.character(variable.names))
         stop("variable.names must be a character vector")
    }
     
    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")
    if (!is.character(type))
      stop("type must be a character vector")

    if (is.null(variable.names)) {
        .Call("set_default_monitors", model$ptr(), as.integer(thin),
              type, PACKAGE="rjags")
    }
    else {
        .Call("set_monitors", model$ptr(), variable.names,
              as.integer(thin), type, PACKAGE="rjags")
    }
    update(model, n.iter)
    ans <- .Call("get_monitored_values", model$ptr(), type, PACKAGE="rjags")
    for (i in seq(along=ans)) {
        class(ans[[i]]) <- "mcarray"
    }
    if (is.null(variable.names)) {
        .Call("clear_default_monitors", model$ptr(), type, PACKAGE="rjags")
    }
    else {
        for (i in seq(along=variable.names)) {
            .Call("clear_monitor", model$ptr(), variable.names[i], type,
                  PACKAGE="rjags")
        }
    }
    return(ans)
}

list.samplers <- function(model)
{
    if (!inherits(model, "jags")) {
        stop("not a jags model object")
    }
    .Call("get_samplers", model$ptr(), PACKAGE="rjags")
}

coda.names <- function(basename, dim)
{
    if (prod(dim) == 1)
      return(basename)
    
    indices <- as.character(1:dim[1])
    if (length(dim) > 1) {
        for (i in 2:length(dim)) {
            indices <- outer(indices, 1:dim[i], FUN=paste, sep=",")
        }
    }
    paste(basename,"[",as.vector(indices),"]",sep="")
}

nchain <- function(model)
{
    if (!inherits(model, "jags"))
      stop("Invalid JAGS model object in nchain")
    
    .Call("get_nchain", model$ptr(), PACKAGE="rjags")
}

coda.samples <- function(model, variable.names=NULL, n.iter, thin=1)
{
    start <- model$iter() + thin
    out <- jags.samples(model, variable.names, n.iter, thin, type="trace")

    ans <- vector("list", nchain(model))
    for (ch in 1:nchain(model)) {
        ans.ch <- vector("list", length(out))

        vnames.ch <- NULL
        for (i in seq(along=out)) {

            varname <- names(out)[[i]]
            d <- dim(out[[i]])
            if (length(d) < 3) {
                stop("Invalid dimensions for sampled output")
            }
            vardim <- d[1:(length(d)-2)]
            nvar <- prod(vardim)
            niter <- d[length(d) - 1]        
            nchain <- d[length(d)]
            
            values <- as.vector(out[[i]])
            var.i <- matrix(NA, nrow=niter, ncol=nvar)
            for (j in 1:nvar) {
                var.i[,j] <- values[j + (0:(niter-1))*nvar + (ch-1)*niter*nvar]
            }
            vnames.ch <- c(vnames.ch, coda.names(varname, vardim))
            ans.ch[[i]] <- var.i
        }
        
        ans.ch <- as.matrix(data.frame(ans.ch))
        colnames(ans.ch) <- vnames.ch
        ans[[ch]] <- mcmc(ans.ch, start=start, thin=thin)
    }
    mcmc.list(ans)
}

jags.module <- function(names, path)
{
    if (missing(path)) {
        path = getOption("jags.moddir")
        if (is.null(path)) {
            stop("option jags.moddir is not set")
        }
    }
    
    cat("loading JAGS module\n")
    for (i in 1:length(names)) {
        cat("   ", names[i], "\n", sep="")
        file <- file.path(path,
                          paste(names[i], .Platform$dynlib.ext, sep=""))
        if (!file.exists(file)) {
            stop("Cannot load ", file)
        }
        dyn.load(file)
    }
}

