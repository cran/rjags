##  R package rjags file R/flags.R
##  Copyright (C) 2006-2026 Martyn Plummer and Matt Denwood
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License version
##  2 as published by the Free Software Foundation.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/
##

pkg.compile.flags <- function(type = c("ALL", "JAGS_ROOT", "JAGS_VERSION", "RJAGS_VERSION", "CPPFLAGS", "CXXFLAGS", "LIBS"), print=type!="ALL")
{

  flags <- get.compile.flags()

  ## For compatibility with JAGS 4->5 transition on macOS only:
  if (.Platform$OS.type=="unix" &&
      as.numeric(flags$JAGS_VERSION[,1])==4 &&
      flags[["CPPFLAGS"]] == "-I/usr/local/include/JAGS" &&
      !dir.exists("/usr/local/include/JAGS") &&
      dir.exists("/opt/jags/versions/jags/current-4/include/JAGS")
  ) {
    flags[["CPPFLAGS"]] <- "-I/opt/jags/versions/jags/current-4/include/JAGS"
    ## Note: do not change JAGS_ROOT etc to preserve equivalence
    ## This does introduce false positive if rjags_4.x is compiled locally
    ## (with /opt/jags/versions/jags/current-4) and and a package module is
    ## compiled on r-universe/CRAN (with /usr/local) but that should be rare
  }
  ## Note: the above code can be removed for rjags_5.x

  type <- match.arg(type)
  value <- c(flags, "ALL"=list(flags))[[type]]

  if (isFALSE(print)) {
    return(value)
  }

  cat(value)
  invisible(value)
}


load.pkg.module <- function(pkgname, libname, bin.compat,
                            quiet=FALSE, check.only=FALSE)
{
  ## Check arguments:
  stopifnot(is.character(pkgname), length(pkgname)==1L, !is.na(pkgname))
  if (!is.list(bin.compat) || !"JAGS_VERSION"%in%names(bin.compat) || !"JAGS_ROOT"%in%names(bin.compat)) {
    stop("The bin.compat argument must be provided: this should be\nsaved within the package namespace using a call to\nrjags:::pkg.compile.flags(\"ALL\") at build time", call.=FALSE)
  }

  ## Check binary compatibility up-front:
  bin.rjags <- pkg.compile.flags("ALL")

  ### Major version of JAGS must match
  maj.vers.pkg <- as.numeric(bin.compat[["JAGS_VERSION"]][,1])
  maj.vers.rjags <- as.numeric(bin.rjags[["JAGS_VERSION"]][,1])
  stopifnot(!is.na(maj.vers.pkg),
            length(maj.vers.pkg)==1L,
            !is.na(maj.vers.pkg))
  if (maj.vers.pkg != maj.vers.rjags) {
    stop("Mismatched JAGS major versions:\n\t", pkgname, ": ", maj.vers.pkg, "\n\trjags: ", maj.vers.rjags, "\n\nThe ", pkgname, " package must either be recompiled or reinstalled\nfollowing recompilation on CRAN.", call.=FALSE)
  }

  ### JAGS_ROOT should match on unix (but not on Windows)
  if (.Platform$OS.type=="unix") {
    jags.root.pkg <- bin.compat[["JAGS_ROOT"]]
    jags.root.rjags <- bin.rjags[["JAGS_ROOT"]]
    stopifnot(is.character(jags.root.pkg),
              !is.na(jags.root.pkg),
              length(jags.root.pkg)==1L)
    if (jags.root.pkg != jags.root.rjags) {
      warning("Mismatched JAGS_ROOT variables:\n\t", pkgname, ": ", jags.root.pkg, "\n\trjags: ", jags.root.rjags, "\n\nLoading duplicate symbols may lead to undefined behaviour.\nThe ", pkgname, " package should probably either be recompiled or\nreinstalled following recompilation on CRAN.", call.=FALSE)
    }
  }

  ### Note: there is scope for additional checks here based on e.g. parsing PKG_LIBS

  ### Primarily for testing purposes:
  if (isTRUE(check.only)) {
    return(invisible())
  }

  ## Extract the relevant libs folder:
  if (missing(libname)){
    path <- system.file(package = pkgname, "libs")
    stopifnot(length(path)==1L)
    if (path=="") {
      stop("Package ", pkgname, " not found", call.=FALSE)
    }
  } else {
    stopifnot(is.character(libname), length(libname)==1L, !is.na(libname))
    path <- file.path(libname, pkgname, "libs")
    stopifnot(length(path)==1L)
    if (!file.exists(path)) {
      stop("Package ", pkgname, " not found in library ", libname, call.=FALSE)
    }
  }
  if (.Platform$r_arch!="") {
    path <- file.path(path, .Platform$r_arch)
  }
  if (!file.exists(file.path(path, paste0(pkgname, .Platform$dynlib.ext)))) {
    stop("File not found: ", file.path(path, paste0(pkgname, .Platform$dynlib.ext)), call.=FALSE)
  }

  ## Try to load the module:
  tryCatch ({
    load.module(pkgname, path, quiet)
  }, error = function(e){
    stop("The ", pkgname, " module failed to load for an unknown reason.\nIt may help to inspect the following PKG_LIB for mismatches:\n\t", pkgname, ": ", bin.compat[["LIBS"]], "\n\trjags: ", bin.rjags[["LIBS"]], "\nEnsure that ", pkgname, " and rjags are compiled against the same build of JAGS.\n\nThe error returned from load.module() was:\n", conditionMessage(e), call.=FALSE)
  })

  invisible()
}
