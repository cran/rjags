## Ensure that binary compatibility passes internally for rjags
library("rjags")
load.pkg.module("test", bin.compat=pkg.compile.flags("ALL"), check.only=TRUE)

## And that it would fail for a different major version:
tcf <- pkg.compile.flags("ALL")
tcf$JAGS_VERSION <- package_version("0.0.0")
ss <- try(load.pkg.module("test", bin.compat=tcf, check.only=TRUE), silent=TRUE)
if (!inherits(ss, "try-error")) {
  stop("Failed to error on incompatible JAGS major versions")
}

## And that it produces a warning for a different JAGS_ROOT (unix only):
if (.Platform$OS.type=="unix") {
  tcf <- pkg.compile.flags("ALL")
  tcf$JAGS_ROOT <- "/some/other/path"
  cw <- options("warn")
  options(warn=2)
  ss <- try(load.pkg.module("test", bin.compat=tcf, check.only=TRUE), silent=TRUE)
  if (!inherits(ss, "try-error")) {
    stop("Failed to error on inconsistent JAGS_ROOT")
  }
  options(warn=cw$warn)
}
