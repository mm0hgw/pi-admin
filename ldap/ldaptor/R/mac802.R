mac802Regex <- "([0123456789ABCDEF])"

#' mac802
#' @description a container for a valid mac802 ethernet address
#' @param x a single value, coerced to numeric
#'@import bit
#'@export
mac802 <- function(mac) {
    out <- as.mac802(mac)
    if (!valid.mac802(out)) {
        print(mac)
        print(out)
        stop()
    }
    class(out) <- "mac802"
    out
}

#' valid.mac802
#' @param x a test object
#'@export
valid.mac802 <- function(x) {
    if (length(x) != 6) 
        return(FALSE)
    if (any(sapply(x, nchar) != 2)) 
        return(FALSE)
    if (gsub(mac802Regex, "", paste(collapse = "", x)) != "") 
        return(FALSE)
    return(TRUE)
}

#'@method as mac802
as.mac802 <- function(x) {
    UseMethod("as.mac802", x)
}

#'@method as.mac802 default
as.mac802.default <- function(x) {
    y <- strsplit(toupper(strsplit(x, "([[:space:]])")[[1]][1]), ":")[[1]]
    print(y)
    if (valid.mac802(y)) {
        class(y) <- "mac802"
        return(y)
    }
    stop(x)
}

#'@method as.mac802 mac802
as.mac802.mac802 <- function(x) {
    x
}

#'@method is mac802
is.mac802 <- function(x) {
    inherits(x, "mac802")
}

#'@method print mac802
print.mac802 <- function(x, ...) {
    cat(format(x), "\n", ...)
}

#'@method format mac802
format.mac802 <- function(x, ...) {
    paste(collapse = ":", x)
}

#'@method == mac802
"==.mac802" <- function(e1, e2) {
    if (format(e1) == format(e2)) 
        return(TRUE)
    return(FALSE)
}

#'valid.mac802list
#' @param x a test object
#'@export
valid.mac802list <- function(x) {
    all(sapply(x, is.mac802))
}

#'mac802list
#' @param x a 'character' filename like '/etc/ethers' 
#' or a 'character' vector like scan('/etc/ethers',what='character',sep='\\n')
#' or a 'list' of 'mac802' objects
#'@export
mac802list <- function(x, ...) {
    UseMethod("mac802list", x)
}

#'@method mac802list list
mac802list.list <- function(x, ...) {
    if (!valid.mac802list(x)) 
        stop(x)
    class(x) <- "mac802list"
    x
}

#'@method mac802list character
mac802list.character <- function(x, ...) {
    if (length(x) == 1 && file.exists(x)) {
        x <- scan(x, what = "character", sep = "\n")
    }
    x <- grep("^#", x, invert = TRUE, value = TRUE)
    x <- strsplit(x, "([[:space:]])")
    print(x)
    templist <- lapply(x, function(y) {
    	
        mac <- mac802(y[1])
        name <- paste(collapse = " ", y[-1])
        list(mac,name)
    })
    print(templist)
    out <- lapply(templist, "[[", "mac")
    names(out) <- sapply(templist, "[[", "name")
    mac802list(out)
}

#'@method format mac802list
format.mac802list <- function(x, ...) {
    paste(collapse = "\n", sapply(seq_along(x), function(i) {
        paste(format(x[[i]]), names(x)[i], sep = "\t")
    }))
}

#'@method print mac802list 
print.mac802list <- print.mac802
