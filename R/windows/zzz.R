supported.versions <- c("1.0.3", "1.0.2")

.onLoad <- function(lib, pkg)
{
    for (i in seq(along=supported.versions)) {
        ## FIXME - We should user SOFTWARE\JAGS\version in future
        key <- paste("SOFTWARE\\JAGS-", supported.versions[i], sep="")
        regkey <- try(readRegistry(key, hive = "HLM", maxdepth = 1),
                      silent = TRUE)
        if(!inherits(regkey, "try-error"))
            break
    }
    if (inherits(regkey, "try-error"))
        stop("Failed to locate a supported JAGS installation")
    
    jags.home <- regkey[["Install_Dir"]]

    ## Add jags.home to the windows PATH, if not already present

    bindir <- file.path(jags.home, "bin")
    path <- Sys.getenv("PATH")
    split.path <- strsplit(path, .Platform$path.sep)$PATH
    if (!any(split.path == bindir)) {
        path <- paste(bindir, path, sep=.Platform$path.sep)
        Sys.setenv("PATH"=path)
    }
    
    ## Set the module directory, if the option jags.moddir is not already set
    
    if (is.null(getOption("jags.moddir"))) {
        options("jags.moddir" = file.path(jags.home, "modules"))
    }
    jags.module(c("basemod","bugs"))
    
    library.dynam("rjags", pkg, lib, local=FALSE)
    .Call("init_jags_console", PACKAGE="rjags")
}
