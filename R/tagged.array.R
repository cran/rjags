dimtags <- function(x)
{
    tags <- attr(x, "dimtags")
    if (is.null(tags)) {
        ## Back-compatibility: In rjags < 5 dimtags of mcarray objects were stored as the
        ## names attribute of the dim attribute
        tags <- names(dim(x))
        if (any(nchar(tags)==0)) {
            ## Value dimensions were implicitly represented by empty strings
            tags[nchar(tags) == 0] <- "value"
        }
    }
    return(tags)
}

`dimtags<-` <- function(x, value)
{
    if (is.null(dim(x))) {
        stop("Cannot set dimtags for object with no dims")
    }
    else if (length(dim(x)) != length(value)) {
        stop("Length mismatch between dimtags and dims")
    }
    attr(x, "dimtags") <- value
    return(x)
}

collapse <- function(object, drop.tags, keep.tags, FUN, tag.result=FALSE, ...)
{
    if (is.null(dim(object)) || is.null(dimtags(object))) {
        stop("Cannot collapse object without both dim and dimtags attributes")
    }

    tags <- dimtags(object)
    if (length(tags) != length(dim(object))) {
        stop("length mismatch between dimtags and dim")
    }
    
    if (!missing(drop.tags) && !missing(keep.tags)) {
        stop("Only one of the arguments 'drop.tags' or 'keep.tags' can be specified")
    }
    else if (!missing(keep.tags)) {
        keep.dims <- tags %in% keep.tags
    }
    else if (!missing(drop.tags)) {
        keep.dims <- !(tags %in% drop.tags)
    }
    else {
        stop("At least one of 'drop.tags' and 'keep.tags' must be specified")
    }
    
    out <- apply(object, MARGIN=which(keep.dims), FUN=FUN, ...)
    if (tag.result && !is.null(dim(out)) && length(dim(out)) == sum(keep.dims)) {
        dimtags(out) <- tags[keep.dims]
    }
    return(out)
}
