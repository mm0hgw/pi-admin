# 32-bit limit
ipv4.allOnes <- 2^33 - 1

#' valid.ipv4
#' @param x a test object
#'@export
valid.ipv4 <- function(x) {
    y <- as.numeric(x)
    if (is.na(y)) 
        return(FALSE)
    if (y < 0) 
        return(FALSE)
    if (y > ipv4.allOnes) 
        return(FALSE)
    return(TRUE)
}

#' as.ipv4
#' @param x a single value, coerced to numeric
#'@export
as.ipv4 <- function(x) {
    y <- as.numeric(x)
    if (valid.ipv4(y)) {
        class(y) <- "ipv4"
        return(y)
    }
    stop(x)
}

#' ipv4
#' @description a container for a valid RFC1918 IPv4 address
#' @param '...' 4 'numeric' or 'integer' type address identifiers
#'@export
ipv4 <- function(x, ...) {
    UseMethod("ipv4", x)
}

#'@method ipv4 integer
ipv4.integer <- function(x, ...) {
    arg <- c(unlist(x), unlist(list(...)))
    stopifnot(length(arg) >= 4)
    vec <- head(arg, n = 4)
    as.ipv4(vec2ip(vec))
}

#'@method ipv4 numeric
ipv4.numeric <- ipv4.integer

#'@method ipv4 list
ipv4.list <- ipv4.integer

#'@method ipv4 ipv4
ipv4.ipv4 <- function(x, ...) {
    if (!valid.ipv4(x)) 
        stop(paste("ipv4 set to", x))
    x
}

#' is.ipv4
#' @param x a test object
#'@export
is.ipv4 <- function(x) {
    inherits(x, "ipv4")
}

vec2ip <- function(vec) {
    sum(sapply(seq(4), function(i) {
        j <- 2^((4 - i) * 8)
        vec[i] * j
    }))
}

ip2vec <- function(ip) {
    sapply(seq(4), function(i) {
        j <- 2^((4 - i) * 8)
        (ip%/%j)%%256
    })
}

#'print.ipv4
#'@param x an 'ipv4' object
#'@param ... passed to print.character
#'@export
print.ipv4 <- function(x, ...) {
    cat(format(x), "\n", ...)
}

#'format.ipv4
#'@param x an 'ipv4' object
#'@param ... ignored
#'@export
format.ipv4 <- function(x, ...) {
    paste(collapse = ".", ip2vec(x))
}

#'+.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object
#'export
"+.ipv4" <- function(e1, e2) {
    as.ipv4(as.numeric(e1) + e2)
}

#'-.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object
#'export
"-.ipv4" <- function(e1, e2) {
    as.ipv4(as.numeric(e1) - e2)
}

#'@import bit
#'@method as.bit ipv4
as.bit.ipv4 <- function(x) {
    out <- bit::bit(32)
    i <- 32
    while (i >= 1) {
        j <- 2^(i - 1)
        if (x >= j) {
            x <- x - j
            out[i] <- TRUE
        }
        i <- i - 1
    }
    out
}

#' valid.ipv4.subnetmask
#' @param x a test object
#'@export
valid.ipv4.subnetmask <- function(x) {
    if (length(x) != 1) 
        return(FALSE)
    if (is.na(x)) 
        return(FALSE)
    if (x < 0 || x > 30) 
        return(FALSE)
    return(TRUE)
    
}

#' valid.ipv4.subnet
#' @param x a test object
#'@export
valid.ipv4.subnet <- function(x) {
    if (typeof(x) != "list") 
        return(FALSE)
    if (!valid.ipv4(x$ip)) 
        return(FALSE)
    if (!valid.ipv4.subnetmask(x$mask)) 
        return(FALSE)
    ipbit <- as.bit(x$ip)
    bitsum <- sum(head(ipbit, n = 32 - x$mask))
    if (bitsum != 0) 
        return(FALSE)
    return(TRUE)
}

#' ipv4.subnet
#' @description a container for a valid RFC1918 IPv4 subnet
#' @param '...' 5 'numeric' or 'integer' type subnet identifiers
#'@export
ipv4.subnet <- function(x, ...) {
    UseMethod("ipv4.subnet", x)
}

#'@method ipv4.subnet integer
ipv4.subnet.integer <- function(x, ...) {
    arg <- c(unlist(x), unlist(list(...)))
    print(arg)
    if (any(is.na(arg))) 
        stop("NA input")
    if (length(arg) < 5) 
        stop(paste("too few arguments,n=", length(arg), sep = ""))
    if (!valid.ipv4.subnetmask(arg[5])) 
        stop(paste("bad subnet size:", arg[5]))
    out <- list(ip = ipv4(arg[1:4]), mask = arg[5])
    class(out) <- c("ipv4.subnet")
    out
}

#'@method ipv4.subnet numeric
ipv4.subnet.numeric <- ipv4.subnet.integer

#'@method ipv4.subnet list
ipv4.subnet.list <- ipv4.subnet.integer

#'@method ipv4.subnet ipv4
ipv4.subnet.ipv4 <- function(x, ...) {
    out <- list()
    out$ip <- x
    out$mask <- c(...)
    if (!valid.ipv4.subnet(out)) 
        stop(match.call())
    class(out) <- "ipv4.subnet"
    out
}

#'@method ipv4.subnet ipv4.subnet
ipv4.subnet.ipv4.subnet <- function(x, ...) {
    if (!valid.ipv4.subnet(x)) 
        stop(match.call())
    x
}

#'print.ipv4.subnet
#'@param x an 'ipv4.subnet' object
#'@param ... passed to print.character
#'@export
print.ipv4.subnet <- function(x, ...) {
    cat(format(x), "\n", ...)
}

#'format.ipv4.subnet
#'@param x an 'ipv4.subnet' object
#'@param ... ignored
#'@export
format.ipv4.subnet <- function(x, ...) {
    print(as.list(x))
    paste(sep = "/", format(x$ip), x$mask)
}

#'ipv4.subnet.netmask
#'@param subnet an 'ipv4.subnet' object
#'@export
ipv4.subnet.netmask <- function(subnet) {
    as.ipv4(ipv4.allOnes - (2^(32 - subnet$mask) - 1))
}

#'ipv4.subnet.broadcast
#'@param subnet an 'ipv4.subnet' object
#'@export
ipv4.subnet.broadcast <- function(subnet) {
    as.ipv4(subnet$ip + 2^(32 - subnet$mask) - 1)
}

#' as.ipv4.subnet
#' @param x a single value, coerced to numeric
#'@export
as.ipv4.subnet <- function(x) {
    y <- as.numeric(x)
    if (valid.ipv4.subnet(y)) {
        class(y) <- "ipv4.subnet"
        return(y)
    }
    stop(x)
}

#' is.ipv4.subnet
#' @param x a test object
#'@export
is.ipv4.subnet <- function(x) {
    inherits(x, "ipv4.subnet")
}

