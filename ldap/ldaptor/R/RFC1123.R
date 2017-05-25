
RFC1123hostregex <- '([a-zA-z0-9\\-])'

host.class <- function(x) {
    stopifnot(is.host.class(x))
    x
}

is.host.class <- function(x) {
    if (!is.character(x)) 
        return(FALSE)
    if (length(x) != 1) 
        return(FALSE)
    if (gsub(RFC1123hostregex,'',x)!='') 
        return(FALSE)
    return(TRUE)
}

domain.class <- function(x) {
    stopifnot(is.domain.class(x))
    x
}

is.domain.class <- function(x) {
    if (!is.character(x)) 
        return(FALSE)
    if (length(x) != 1) 
        return(FALSE)
    dots <- strsplit(gsub(RFC1123hostregex,'',x),'')[[1]]
    if (length(dots)==0 || any(dots != '.'))
        return(FALSE)
    return(TRUE)
}
