#' ip.class
#' @description a container for a valid RFC1918 IPv4 address
#' @param hostname a 'character' identifier
#'@export
ip.class <- function(...) {
    out <- as.integer(c(...))
    stopifnot(length(out) >= 4)
    out <- head(out, n = 4)
    stopifnot(!any(is.na(out)))
    stopifnot(all(out >= 0))
    stopifnot(all(out <= 255))
    class(out) <- "ip.class"
    out
}

#'format.ip.class
#'@param x an 'ip.class' object
#'@param ... ignored
#'@export
format.ip.class <- function(x, ...) {
    paste(collapse = ".", x)
}

#'+.ip.class
#'@param e1 an 'ip.class' object
#'@param e2 an 'ip.class' object or length==1 integer
#'@export
"+.ip.class" <- function(e1, e2) {
    if (!inherits(e1, "ip.class")) 
        stop(e1)
    if (!inherits(e2, "ip.class")) {
        if (length(e2) != 1) 
            stop(e2)
        i <- as.integer(e2)
        if (is.na(i)) 
            stop(i)
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
    stopifnot(ip[1] >= 0 && ip[1] <= 255)
    ip
}
