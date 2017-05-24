
RFC2253special <- c(",", "+", "\"", "<", ">", ";")
RFC2253specialregex <- paste(collapse = "", c("([", RFC2253special, "])"))
RFC2253chars <- c(RFC1123chars, ".", RFC2253special)

ldapkv <- function(key, value) {
    stopifnot(is.character(key))
    stopifnot(length(key) == 1)
    stopifnot(length(setdiff(strsplit(key, "")[[1]], RFC2253chars)) == 0)
    stopifnot(is.character(value))
    stopifnot(length(value) == 1)
    stopifnot(length(setdiff(strsplit(value, "")[[1]], RFC2253chars)) == 0)
    out <- c(key, value)
    class(out) <- "ldapkv"
    out
}


format.ldapkv <- function(x, sep = ": ", ...) {
    gsub(RFC2253specialregex, "\\\\\\1", paste(collapse = sep, x))
}

is.ldapkv <- function(x) {
    inherits(x, "ldapkv")
}



ldapquery <- function(pkey, basedn, skeylist = list(), kvlist = list()) {
    stopifnot(is.ldapkv(pkey))
    if (is.domain.class(basedn)) 
        basedn <- basedn.class(basedn)
    stopifnot(is.basedn.class(basedn))
    stopifnot(all(sapply(c(skeylist, kvlist), is.ldapkv)))
    
    out <- list(pkey = pkey, basedn = basedn, skeylist = skeylist, kvlist = kvlist)
    class(out) <- "ldapquery"
    out
}

format.ldapquery <- function(x, ...) {
    dnlist <- c(x$pkey, x$skeylist, x$basedn)
    dn <- sapply(dnlist, format, sep = "=")
    format(ldapkv("dn", paste(collapse = ",", dn)))
    
}

is.ldapquery <- function(x) {
    inherits(x, "ldapquery")
}


is.basedn.class <- function(x) {
    inherits(x, "basedn.class")
}

basedn.class <- function(domain) {
    stopifnot(is.domain.class(domain))
    dcs <- strsplit(domain, "\\.")[[1]]
    out <- lapply(dcs, ldapkv, key = "dc")
    class(out) <- "basedn.class"
    out
}

format.basedn <- function(x, ...) {
    dn <- sapply(x, format, sep = "=")
    format(ldifkv("dn", paste(collapse = ",", dn)))
}

