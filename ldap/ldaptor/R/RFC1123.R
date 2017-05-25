
#'RFC1123HostnameRegex
#'@description A regex to identify valid RFC1123 hostname characters
#'@export
RFC1123HostnameRegex <- "([a-zA-z0-9\\-])"

#' hostname.class
#' @description a container for a valid RFC1123 hostname
#' @param hostname a 'character' identifier
#'@export
hostname.class <- function(hostname) {
    stopifnot(valid.hostname.class(hostname))
    class(hostname) <- "hostname.class"
    hostname
}

#' is.hostname.class
#' @param x test object
#'@export
is.hostname.class <- function(x) {
    inherits(x, "hostname.class")
}

#' valid.hostname.class
#' @param x test object
#'@examples
#'hexchars <-'123456789ABCDEF'
#'stopifnot(valid.hostname.class(vector())==FALSE)
#'stopifnot(valid.hostname.class('')==FALSE)
#'stopifnot(valid.hostname.class(paste(collapse='',rep(4,hexchars)))==FALSE)
#'stopifnot(valid.hostname.class(':')==FALSE)
#'stopifnot(valid.hostname.class(paste(collapse='',c(letters,LETTERS,'-')))==TRUE)
#'@export
valid.hostname.class <- function(x) {
    x <- as.character(x)
    if (length(x) != 1) 
        return(FALSE)
    if (nchar(x) == 0 || nchar(x) > 63) 
        return(FALSE)
    if (gsub(RFC1123HostnameRegex, "", x) != "") 
        return(FALSE)
    return(TRUE)
}

#' domain.class
#' @description a container for a valid RFC1123 domaim name
#' @param domain a 'character' identifier
#'@export
domain.class <- function(domain) {
    stopifnot(valid.domain.class(domain))
    class(domain) <- "domain.class"
    domain
}

#' is.domain.class
#' @param x test object
#'@export
is.domain.class <- function(x) {
    inherits(x, "domain.class")
}

#' valid.domain.class
#' @param x test object
#'@examples
#'hexchars <-'123456789ABCDEF'
#'stopifnot(valid.hostname.class(vector())==FALSE)
#'stopifnot(valid.hostname.class('')==FALSE)
#'stopifnot(valid.hostname.class(paste(collapse='',rep(16,hexchars)))==FALSE)
#'stopifnot(valid.hostname.class(':')==FALSE)
#'stopifnot(valid.hostname.class(paste(collapse='',c(letters,,'.',LETTERS,'-')))==TRUE)
#'@export
valid.domain.class <- function(x) {
    x <- as.character(x)
    if (length(x) != 1) 
        return(FALSE)
    if (nchar(x) == 0 || nchar(x) > 253) 
        return(FALSE)
    dcs <- strsplit(x, ".")[[1]]
    if (all(sapply(dcs, valid.hostname.class))) 
        return(FALSE)
    return(TRUE)
}
