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
ipv4 <- function(x,...) {
	UseMethod('ipv4',x)
}

#'@method ipv4 integer
ipv4.integer <- function(x,...){	
    arg <- c(unlist(x),unlist(list(...)))
    stopifnot(length(arg) >= 4)
    vec <- head(arg, n = 4)
    as.ipv4(vec2ip(vec))
}

#'@method ipv4 numeric
ipv4.numeric <- ipv4.integer

#'@method ipv4 list
ipv4.list <- ipv4.integer

#'@method ipv4 ipv4
ipv4.ipv4 <- function(x,...){
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

#' ipv4.subnet
#' @description a container for a valid RFC1918 IPv4 subnet
#' @param '...' 5 'numeric' or 'integer' type subnet identifiers
#'@export
ipv4.subnet <- function(...) {
    arg <- as.integer(c(...))
    stopifnot(length(arg) >= 5)
    stopifnot(arg[5] >= 0)
    stopifnot(arg[5] <= 30)
    stopifnot(!any(is.na(arg)))
    ip <- ipv4(arg)
    out <- list(ip = ip, mask = arg[5])
    class(out) <- c("ipv4.subnet")
    out
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
    paste(sep = "/", format(x$ip), x$mask)
}

#'ipv4.subnet.netmask
#'@param subnet an 'ipv4.subnet' object
#'@export
ipv4.subnet.netmask <- function(subnet) {
    as.ipv4((2^33 - 1) - (2^(32 - subnet$mask) - 1))
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
