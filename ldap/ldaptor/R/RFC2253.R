

#'RFC2253Regex
#'@description A regex to identify valid RFC1123 string characters
#'@export
RFC2253Regex <- "([a-zA-z0-9\\-.,\\+\\\"\\<>;])"


RFC2253Special <- c(",", "+", "\"", "\\", "<", ">", ";")

Rspecialregex <- c("([\\\"\\\\\\-\\+])")


#' RFC2253string
#' @description a container for a valid RFC2253 string
#' @param string a 'character' string
#'@export
RFC2253string <- function(string) {
    stopifnot(valid.RFC2253string(string))
    class(string) <- "RFC2253string"
    string
}

#' is.RFC2253string
#' @param x test object
#'@export
is.RFC2253string <- function(x) {
    inherits(x, "RFC2253string")
}

#' valid.RFC2253string
#' @param x test object
#'@export
valid.RFC2253string <- function(x) {
    if (!is.character(x)) 
        return(FALSE)
    if (length(x) != 1) 
        return(FALSE)
    if (gsub(RFC2253Regex, "", x) != "") 
        return(FALSE)
    return(TRUE)
}

#' ldapkv
#' @description a container for a valid LDAP key/value pair
#' @param key a valid 'RFC2253string'
#' @param value a valid 'RFC2253string'
#'@export
ldapkv <- function(key, value) {
    out <- c(RFC2253string(key), RFC2253string(value))
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
format.ldapkv <- function(x, sep = ": ", ...) {
    gsub(RFC2253specialregex, "\\\\\\1", paste(collapse = sep, x))
}



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

format.ldapquery <- function(x, ...) {
    dnlist <- c(list(x$pkey), x$skeylist, x$basedn)
    dn <- ldapkv("dn", paste(collapse = ",", sapply(dnlist, format, sep = "=")))
    querylist <- c(list(dn), list(x$pkey), x$kvlist)
    querylist
}

is.ldapquery <- function(x) {
    inherits(x, "ldapquery")
}

is.basedn.class <- function(x) {
    inherits(x, "basedn.class")
}

basedn.class <- function(domain) {
    stopifnot(valid.domain.class(domain))
    dcs <- strsplit(domain, "\\.")[[1]]
    out <- lapply(dcs, ldapkv, key = "dc")
    class(out) <- "basedn.class"
    out
}

format.basedn <- function(x, ...) {
    dn <- sapply(x, format, sep = "=")
    format(ldifkv("dn", paste(collapse = ",", dn)))
}

