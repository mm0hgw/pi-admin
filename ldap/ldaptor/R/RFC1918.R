#' ipv4
#' @description a container for a valid RFC1918 IPv4 address
#' @param '...' 4 'numeric' or 'integer' type address identifiers
#'@export
ipv4 <- function(...) {
    out <- as.integer(c(...))
    stopifnot(length(out) >= 4)
    out <- head(out, n = 4)
    stopifnot(!any(is.na(out)))
    stopifnot(all(out >= 0))
    stopifnot(all(out <= 255))
    class(out) <- "ipv4"
    out
}

#'format.ipv4
#'@param x an 'ipv4' object
#'@param ... ignored
#'@export
format.ipv4 <- function(x, ...) {
    paste(collapse = ".", x)
}

#'+.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object or length==1 integer
#'@export
"+.ipv4" <- function(e1, e2) {
    if (!inherits(e1, "ipv4")) 
        stop(e1)
    if (!inherits(e2, "ipv4")) {
        if (length(e2) != 1) 
            stop(paste(collapse = ",", e2))
        i <- as.integer(e2)
        if (is.na(i)) 
            stop(e2)
        j <- e1
        j[4] <- j[4] + i
        out <- handleOverflow(j)
    } else {
        out <- handleOverflow(as.vector(e1) + as.vector(e2))
    }
}
handleOverflow <- function(ip) {
    i <- 4
    while (i > 1) {
        if (ip[i] > 255 || ip[i] < 0) {
            j <- ip[i]%/%255
            ip[i] <- ip[i] - j * 255
            ip[i - 1] <- ip[i - 1] + j
        }
        i <- i - 1
    }
    if (ip[1] < 0 && ip[1] > 255) 
        stop(ip)
    ipv4(ip)
}

#'-.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object or length==1 integer
#'@export
"-.ipv4" <- function(e1, e2) {
    e1 + (-e2)
}

#'==.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object
#'@export
"==.ipv4" <- function(e1, e2) {
    if (!inherits(e2, "ipv4")) 
        stop(e2)
    all(as.vector(e1) == as.vector(e2))
}

#'>.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object
#'@export
"<.ipv4" <- function(e1, e2) {
    if (!inherits(e2, "ipv4")) 
        stop(e2)
    i <- 1
    while (i <= 4) {
        if (e1[i] < e2[i]) 
            return(TRUE)
        if (e1[i] > e2[i]) 
            return(FALSE)
        i <- i + 1
    }
    return(FALSE)
}

#'<.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object
#'@export
">.ipv4" <- function(e1, e2) {
    if (!inherits(e2, "ipv4")) 
        stop(e2)
    i <- 1
    while (i <= 4) {
        if (e1[i] > e2[i]) 
            return(TRUE)
        if (e1[i] < e2[i]) 
            return(FALSE)
        i <- i + 1
    }
    return(FALSE)
}

#'<=.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object
#'@export
"<=.ipv4" <- function(e1, e2) {
    if (e1 == e2) 
        return(TRUE)
    e1 < e2
}

#'>=.ipv4
#'@param e1 an 'ipv4' object
#'@param e2 an 'ipv4' object
#'@export
">=.ipv4" <- function(e1, e2) {
    if (e1 == e2) 
        return(TRUE)
    e1 < e2
}

#' ipv4.subnet
#' @description a container for a valid RFC1918 IPv4 subnet
#' @param '...' 5 'numeric' or 'integer' type subnet identifiers
#'@export
ipv4.subnet <- function(...) {
    arg <- as.integer(c(...))
    stopifnot(length(out) >= 5)
    out <- head(out, n = 5)
    stopifnot(!any(is.na(out)))
    stopifnot(all(out[1:4] >= 0))
    stopifnot(all(out[1:4] <= 255))
    stopifnot(out[5] >= 0)
    stopifnot(out[5] <= 30)
    stopifnot(!any(is.na(out)))
    class(out) <- c("ipv4.subnet")
    out
}
