
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
#'@export
valid.hostname.class <- function(x) {
    if (!is.character(x)) 
        return(FALSE)
    if (length(x) != 1) 
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

#'valid.domain.class
#'@export
valid.domain.class <- function(x) {
    if (!is.character(x)) 
        return(FALSE)
    if (length(x) != 1) 
        return(FALSE)
    dots <- strsplit(gsub(RFC1123HostnameRegex, "", x), "")[[1]]
    if (length(dots) == 0 || any(dots != ".")) 
        return(FALSE)
    return(TRUE)
}
