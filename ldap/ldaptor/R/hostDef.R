
#'@method valid hostDef
valid.hostDef <- function(x) {
    if (!is.list(x)) 
        return(FALSE)
    if (!valid.hostname.class(x$hostname)) 
        return(FALSE)
    if (!is.null(x$mac) && !valid.mac802(x$mac)) 
        return(FALSE)
    return(TRUE)
}

#'@method as hostDef
as.hostDef <- function(x, ...) {
    UseMethod("as.hostDef", x)
}

#'@method as.hostDef default
as.hostDef.default <- function(x) {
    if (!valid.hostDef(x)) 
        stop()
    class(x) <- "hostDef"
    x
}

#'hostDef
#'@export
hostDef <- function(hostname, ...) {
    as.hostDef(as.list(match.call())[-1])
}
