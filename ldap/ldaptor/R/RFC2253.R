# RFC2253 special characters c(',', '+', '\'', '\\', '<', '>', ';')

#' RFC2253Character
#' @description a container RFC2253 character data 
#' @param string a 'character' string
#'@export
RFC2253Character <- function(string) {
    stopifnot(valid.RFC2253Character(string))
    class(string) <- "RFC2253Character"
    string
}

#'@method is RFC2253Character
is.RFC2253Character <- function(x) {
    inherits(x, "RFC2253Character")
}

#'@method valid RFC2253Character
valid.RFC2253Character <- function(x) {
    x <- as.character(x)
    if (any(gsub(RFC2253Regex, "", x) != "")) 
        stop(x)
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
    stopifnot(length(key) == 1)
    stopifnot(length(value) == 1)
    out <- RFC2253Character(c(key, value))
    class(out) <- "ldapkv"
    out
}

#'@method is ldapkv
is.ldapkv <- function(x) {
    inherits(x, "ldapkv")
}

#'@method format ldapkv
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
    if (!all(sapply(c(skeylist, kvlist), is.ldapkv))) {
        errors <- ""
        if (any(!sapply(skeylist, is.ldapkv))) {
            errors <- paste(sep = "", errors, "skeylist errors: ", paste(collapse = ",", 
                skeylist[!sapply(skeylist, is.ldapkv)]), " ")
        }
        if (any(!sapply(kvlist, is.ldapkv))) {
            errors <- paste(sep = "", errors, "skeylist errors: ", paste(collapse = ",", 
                kvlist[!sapply(kvlist, is.ldapkv)]), " ")
        }
        stop(errors)
    }
    
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

#'@method is ldapquery
is.ldapquery <- function(x) {
    inherits(x, "ldapquery")
}

#'@method is basedn.class
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
    paste(collapse = ",", sapply(x, format, collapse = "="))
}

#'@method is ldapquerylist
is.ldapquerylist <- function(x) {
    inherits(x, "ldapquerylist")
}

#'@method valid ldapquerylist
valid.ldapquerylist <- function(x) {
    if (!all(sapply(x, is.ldapquery))) {
        return(FALSE)
    }
    return(TRUE)
}

#'ldapquerylist
#'@param ... either 1 'list' of 'ldapquery' objects, or 
#'arbitrary 'ldapquery' objects
#'@export
ldapquerylist <- function(...) {
    out <- list(...)
    if (length(out) != 0 && !inherits(out[[1]], "ldapquery")) {
        out <- out[[1]]
    }
    if (!valid.ldapquerylist(out)) 
        stop(out)
    class(out) <- "ldapquerylist"
    
    out
}

#'@method format ldapquerylist
format.ldapquerylist <- function(x, ...) {
    paste(collapse = "\n", sapply(x, format))
}

#'@method + ldapquerylist
"+.ldapquerylist" <- function(e1, e2) {
    UseMethod("+.ldapquerylist", e2)
}

# @method +.ldapquerylist ldapquery
"+.ldapquerylist.ldapquery" <- function(e1, e2) {
    ldapquerylist(append(e1, list(e2)))
}

# @method +.ldapquerylist ldapquerylist
"+.ldapquerylist.ldapquerylist" <- function(e1, e2) {
    ldapquerylist(append(e1, e2))
}

#'@method is ldapkvlist
is.ldapkvlist <- function(x) {
    inherits(x, "ldapkvlist")
}

#'@method valid ldapkvlist
valid.ldapkvlist <- function(x) {
    if (!all(sapply(x, is.ldapkv))) {
        return(FALSE)
    }
    return(TRUE)
}

#'ldapkvlist
#'@param ... either 1 'list' of 'ldapkv' objects, or 
#'arbitrary 'ldapkv' objects
#'@export
ldapkvlist <- function(...) {
    out <- list(...)
    if (!inherits(out[[1]], "ldapkv")) {
        out <- out[[1]]
    }
    if (!valid.ldapkvlist(out)) 
        stop(out)
    class(out) <- "ldapkvlist"
    
    out
}

#'@method format ldapkvlist
format.ldapkvlist <- function(x, ...) {
    paste(collapse = "\n", sapply(x, format))
}

#'@method + ldapkvlist
"+.ldapkvlist" <- function(e1, e2) {
    UseMethod("+.ldapkvlist", e2)
}

# @method +.ldapkvlist ldapkv
"+.ldapkvlist.ldapkv" <- function(e1, list(e2)) {
    ldapkvlist(append(e1, list(e2)))
}

# @method +.ldapkvlist ldapkvlist
"+.ldapkvlist.ldapkvlist" <- function(e1, e2) {
    ldapkvlist(append(e1, e2))
}
