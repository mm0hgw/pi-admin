
# RFC1123

RFC1123chars <- c(letters, LETTERS, "-", seq(0, 9))

host.class <- function(x) {
    stopifnot(is.host.class(x))
    x
}

is.host.class <- function(x) {
    if (!is.character(x)) 
        return(FALSE)
    if (length(x) != 1) 
        return(FALSE)
    hostchars <- strsplit(x, "")[[1]]
    if (length(setdiff(hostchars, RFC1123chars)) != 0) 
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
    dcs <- strsplit(x, "\\.")[[1]]
    if (length(dcs) < 2 || !all(sapply(dcs, is.host.class))) 
        return(FALSE)
    return(TRUE)
}
