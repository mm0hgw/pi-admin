#'@method valid ipv4
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

#'@method as ipv4
as.ipv4 <- function(x) {
    UseMethod("as.ipv4", x)
}

#'@method as.ipv4 default
as.ipv4.default <- function(x) {
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

#'@method ipv4 character
ipv4.character <- function(x, ...) {
    ipv4(as.integer(strsplit(x, "\\.")[[1]]))
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

#'@method is ipv4
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

#'@method print ipv4
print.ipv4 <- function(x, ...) {
    cat(format(x), "\n", ...)
}

#'@method format ipv4
format.ipv4 <- function(x, ...) {
    paste(collapse = ".", ip2vec(x))
}

#'@method + ipv4
"+.ipv4" <- function(e1, e2) {
    as.ipv4(as.numeric(e1) + e2)
}

#'@method - ipv4
"-.ipv4" <- function(e1, e2) {
    as.ipv4(as.numeric(e1) - e2)
}

#'@method valid ipv4.subnetmask
valid.ipv4.subnetmask <- function(x) {
    if (length(x) != 1) 
        return(FALSE)
    if (is.na(x)) 
        return(FALSE)
    if (x < 0 || x > 30) 
        return(FALSE)
    return(TRUE)
    
}

#'@method as.ipv4 bit
as.ipv4.bit <- function(x, ...) {
    if (length(x) != 32) 
        stop("32 bits to an ipv4")
    as.ipv4(sum(sapply(seq(32), function(i) {
        if (x[i]) {
            2^(i - 1)
        } else {
            0
        }
    })))
}

#'@import bit
#'@method as.bit ipv4
as.bit.ipv4 <- function(x, ...) {
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

#'@method valid ipv4.subnet
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

#'@method ipv4.subnet character
ipv4.subnet.character <- function(x, ...) {
    ipv4.subnet(as.integer(strsplit(x, "([\\./]).")[[1]]))
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

#'@method print ipv4.subnet
print.ipv4.subnet <- print.ipv4

#'@method format ipv4.subnet
format.ipv4.subnet <- function(x, ...) {
    paste(sep = "/", format(x$ip), x$mask)
}

#'netmask
#'@param subnet an 'ipv4.subnet' object
#'@export
netmask <- function(subnet) {
    as.ipv4(ipv4.allOnes - (2^(32 - subnet$mask) - 1))
}

#'broadcast
#'@param subnet an 'ipv4.subnet' object
#'@export
broadcast <- function(subnet) {
    as.ipv4(subnet$ip + 2^(32 - subnet$mask) - 1)
}

#'@method as ipv4.subnet
as.ipv4.subnet <- function(x) {
    y <- as.numeric(x)
    if (valid.ipv4.subnet(y)) {
        class(y) <- "ipv4.subnet"
        return(y)
    }
    stop(x)
}

#'@method is ipv4.subnet
is.ipv4.subnet <- function(x) {
    inherits(x, "ipv4.subnet")
}

#'@method valid ipv4list
valid.ipv4list <- function(x) {
    if (!identical(typeof(x), "list")) 
        return(FALSE)
    all(sapply(x, is.ipv4))
}

#'ipv4list
#' @param x a 'character' filename like '/etc/hosts' 
#' or a 'character' vector like scan('/etc/hosts',what='character',sep='\\n')
#' or a 'list' of 'ipv4' objects
#'@export
ipv4list <- function(x, ...) {
    UseMethod("ipv4list", x)
}

#'@method ipv4list list
ipv4list.list <- function(x, ...) {
    if (!valid.ipv4list(x)) 
        stop(x)
    class(x) <- "ipv4list"
    x
}

#'@method ipv4list character
ipv4list.character <- function(x, ...) {
    if (length(x) == 1 && file.exists(x)) {
        x <- scan(x, what = "character", sep = "\n")
    }
    x <- grep("^#", x, invert = TRUE, value = TRUE)
    x <- grep("^f", x, invert = TRUE, value = TRUE)  #skip ipv6
    x <- strsplit(x, "([[:space:]])")
    templist <- lapply(x, function(y) {
        t1 <- strsplit(y[1], "\\.")[[1]]
        print(t1)
        if (identical(length(t1), 4)) {
        	
            ip <- ipv4(y[1])
            name <- paste(collapse = " ", y[-1])
            list(data = TRUE, ip = ip, name = name)
        } else {
            list(data = FALSE)
        }
    })
    print(templist)
    templist <- templist[sapply(templist, "[[", "data")]
    out <- lapply(templist, "[[", "ip")
    names(out) <- sapply(templist, "[[", "name")
    ipv4list(out)
}

#'@method format ipv4list
format.ipv4list <- function(x, ...) {
    paste(collapse = "\n", sapply(seq_along(x), function(i) {
        paste(format(x[[i]]), names(x)[i], sep = "\t")
    }))
}

#'@method print ipv4list 
print.ipv4list <- print.ipv4

#'@method valid ipv4.subnetlist
valid.ipv4.subnetlist <- function(x) {
    all(sapply(x, is.ipv4.subnet))
}

#'ipv4.subnetlist
#' @param x a 'character' filename like '/etc/networks' 
#' or a 'character' vector like scan('/etc/networks',what='character',sep='\\n')
#' or a 'list' of 'ipv4.subnet' objects
#'@export
ipv4.subnetlist <- function(x, ...) {
    UseMethod("ipv4.subnetlist", x)
}

#'@method ipv4list list
ipv4.subnetlist.list <- function(x, ...) {
    if (!valid.ipv4.subnetlist(x)) 
        stop(x)
    class(x) <- "ipv4.subnetlist"
    x
}

#'@method ipv4.subnetlist character
ipv4.subnetlist.character <- function(x, ...) {
    if (length(x) == 1 && file.exists(x)) {
        x <- scan(x, what = "character", sep = "\n")
    }
    x <- grep("^#", x, invert = TRUE, value = TRUE)
    x <- strsplit(x, "([[:space:]])")
    templist <- lapply(x, function(y) {
        ip <- ipv4.subnet(y[1])
        name <- paste(collapse = " ", y[-1])
    })
    out <- lapply(templist, "[[", "ip")
    names(out) <- sapply(templist, "[[", "name")
    valid.ipv4.subnetlist(out)
}

#'@method print ipv4.subnetlist 
print.ipv4.subnetlist <- print.ipv4list

#'@method format ipv4.subnetlist 
format.ipv4.subnetlist <- format.ipv4list
