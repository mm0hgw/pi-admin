# RFC2253 special characters c(',', '+', '\'', '\\', '<', '>', ';')

#'RFC2253Regex
#'@description A regex to identify valid RFC2253 string characters
#'@export
RFC2253Regex <- "([a-zA-z0-9\\-\\.,+\\\"\\\\<>;])"

#'RFC2253SpecialRegex
#'@description A regex to identify valid RFC2253 special characters
#'@export
RFC2253SpecialRegex <- "([,+\\\"\\\\<>;])"

#' RFC2253Character
#' @description a container RFC2253 character data 
#' @param string a 'character' string
#'@export
RFC2253Character <- function(string) {
    stopifnot(valid.RFC2253Character(string))
    class(string) <- "RFC2253Character"
    string
}

#' is.RFC2253Character
#' @param x test object
#'@export
is.RFC2253Character <- function(x) {
    inherits(x, "RFC2253Character")
}

#' valid.RFC2253Character
#' @param x test object
#'@export
valid.RFC2253Character <- function(x) {
    x <- as.character(x)
    if (any(gsub(RFC2253Regex, "", x) != "")) 
        return(FALSE)
    return(TRUE)
}

#'@method format RFC2253Character
format.RFC2253Character <- function(x, ...) {
    gsub(RFC2253SpecialRegex, "\\\\\\1", x)
}

#' ldapkv
#' @description a container for a valid LDAP key/value pair
#' @param key a valid 'RFC2253Character'
#' @param value a valid 'RFC2253Character'
#'@export
ldapkv <- function(key, value) {
	stopifnot(length(key)==1)
	stopifnot(length(value)==1)
    out <- RFC2253Character(c(key, value))
    class(out) <- "ldapkv"
    out
}

#'is.ldapkv
#' @param x test object
#'@export
is.ldapkv <- function(x) {
    inherits(x, "ldapkv")
}

#'format.ldapkv
#'@param x ldapkv object
#'@param sep the 'character' used to divide key and value 
#'@export
format.ldapkv <- function(x, collapse = ": ", ...) {
    paste(collapse = collapse, sapply(x, format.RFC2253Character))
}

#' ldapquery
#' @description a container for a valid LDAP query
#' @param pkey a 'ldapkv' the primary key for the query
#' @param basedn a valid 'domain.class' or a 'basedn.class' the base dn for the query
#' @param skeylist a 'list' of 'ldapkv' objects (default==0) the secondary keys for the query
#' @param kvlist a 'list' of 'ldapkv' objects (default==0) the content of the query
#'@export
ldapquery <- function(pkey, basedn, skeylist = list(), kvlist = list()) {
    stopifnot(is.ldapkv(pkey))
    if (valid.domain.class(basedn)) 
        basedn <- basedn.class(basedn)
    stopifnot(is.basedn.class(basedn))
    stopifnot(all(sapply(c(skeylist, kvlist), is.ldapkv)))
    
    out <- list(pkey = pkey, basedn = basedn, skeylist = skeylist, kvlist = kvlist)
    class(out) <- "ldapquery"
    out
}

#'@method format ldapquery
format.ldapquery <- function(x, ...) {
    dnlist <- c(list(x$pkey), x$skeylist, x$basedn)
    dn <- paste("dn:", paste(collapse = ",", sapply(dnlist, format, collapse = "=")))
    qlist <- sapply(c(list(x$pkey), x$kvlist), format)
    paste(collapse = "\n", c(dn, qlist, ""))
}

#'is.ldapquery
#' @param x test object
#'@export
is.ldapquery <- function(x) {
    inherits(x, "ldapquery")
}

#'is.basedn.class
#' @param x test object
#'@export
is.basedn.class <- function(x) {
    inherits(x, "basedn.class")
}

#'basedn.class
#'@description A container for a LDAP basedn
#' @param domain a valid 'domain.class'
#'@export
basedn.class <- function(domain) {
    stopifnot(valid.domain.class(domain))
    dcs <- strsplit(domain, "\\.")[[1]]
    out <- lapply(dcs, ldapkv, key = "dc")
    class(out) <- "basedn.class"
    out
}

#'@method format basedn.class
format.basedn.class <- function(x, ...) {
    paste(collapse = ",", sapply(dnlist, format, collapse = "="))
}
