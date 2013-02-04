.findJAGS <- function(hive, major)
{
  ## Returns the registry key corresponding to the latest release of
  ## JAGS-major.x.y, or NULL if no release is found
  
  regkey <- try(readRegistry("SOFTWARE\\JAGS", hive = hive, maxdepth = 2,
                             view="32-bit"), silent = TRUE)
  if (inherits(regkey, "try-error")) {
    return(NULL)
  }
  keynames <- names(regkey)
  keynames <- keynames[grep(paste("^JAGS-", major, "\\.", sep=""), keynames)]
  if (length(keynames) == 0) {
    return(NULL)
  }
  else {
    keynames <- rev(keynames) #Search in reverse order of release number
    regkey <- regkey[keynames]
    for (i in seq(along=keynames)) {
      if (!is.null(regkey[[i]][["InstallDir"]])) {
        return(regkey[i])
      }
    }
    return(NULL)
  }
}

.noJAGS <- function(major)
{
  paste("Failed to locate any version of JAGS version ", major, "\n\n",
        "The rjags package is just an interface to the JAGS library\n",
        "Make sure you have installed JAGS-", major,
        ".0.0.exe or higher from\n",
        "http://www.sourceforge.net/projects/mcmc-jags/files\n", sep="")
}

.onLoad <- function(lib, pkg)
{
### First task is to get installation directory of JAGS

    ## Try environment variable first
    jags.home <- Sys.getenv("JAGS_HOME")
    if (nchar(jags.home)==0) {
      ## Search the registry. We need to look for both machine-wide and
      ## user-specific installations

      jags.major <- 3
      
      key1 <- .findJAGS("HLM", jags.major)
      key2 <- .findJAGS("HCU", jags.major)

      if (is.null(key1)) {
        if (is.null(key2)) {
          stop(.noJAGS(jags.major))
        }
        else {
          latest <- key2
        }
      }
      else if (is.null(key2) || names(key2) < names(key1)) {
        latest <- key1
      }
      else {
        latest <- key2
      }

      jags.home <- latest[[1]][["InstallDir"]]
    }
    
### Add jags.home to the windows PATH, if not already present

    bindir <- file.path(jags.home, .Platform$r_arch, "bin")
    path <- Sys.getenv("PATH")
    split.path <- strsplit(path, .Platform$path.sep)$PATH
    if (!any(split.path == bindir)) {
        path <- paste(bindir, path, sep=.Platform$path.sep)
        Sys.setenv("PATH"=path)
    }
    
### Set the module directory, if the option jags.moddir is not already set
    
    if (is.null(getOption("jags.moddir"))) {
        options("jags.moddir" = file.path(jags.home, .Platform$r_arch,
                "modules"))
    }
    library.dynam("rjags", pkg, lib)
    load.module("basemod", quiet=TRUE)
    load.module("bugs", quiet=TRUE)

### Set progress bar type
    
    if (is.null(getOption("jags.pb"))) {
        options("jags.pb"="text")
    }
}

.onAttach <- function(lib, pkg)
{
    packageStartupMessage("Linked to JAGS ",
                          .Call("get_version", PACKAGE="rjags"))
    packageStartupMessage("Loaded modules: ",
                          paste(list.modules(), collapse=","))
}


.onUnload <- function(libpath)
{
    library.dynam.unload("rjags", libpath)
}
