#  R package rjags file R/utilities.R
#  Copyright (C) 2006-2026 Martyn Plummer and Matt Denwood
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

# Note: all functions in here are exported as of rjags_5-4

jags.version <- function(){
    vers <- .Call("get_version", PACKAGE="rjags")
    ## Remove potential build information:
    vers <- sub(" (.*)$", "", vers)
    ## Work around bug introduced by 5.0.0-beta as package name
    vers <- sub("-beta$", "", vers)
    package_version(vers)
}

observed.stochastic.nodes <- function(model){
    if (!inherits(model, "jags")) {
        stop("Invalid JAGS model")
    }
    .Call("get_obs_stoch_names", model$ptr(), PACKAGE="rjags")
}

node.names <- function(model){
    if (!inherits(model, "jags")) {
        stop("Invalid JAGS model")
    }
    .Call("get_variable_names", model$ptr(), PACKAGE="rjags")
}

expand.name <- function(name, dimensions){
    ## A wrapper around coda.names with additional argument checking
    if (!is.character(name) || length(name) != 1L || is.na(name) || name=="") {
      stop("name must be a non-empty character of length 1")
    }
    if (!is.numeric(dimensions) || !all(is.finite(dimensions)) || !all(dimensions>0L) || !all(dimensions%%1==0)) {
      stop("dimensions must be vector of strictly positive (finite) integers")
    }
    coda.names(name, dimensions)
}
