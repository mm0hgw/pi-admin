mac802Regex <- "([0123456789ABCDEF])"

#' mac802
#' @description a container for a valid mac802 ethernet address
#' @param x a single value, coerced to numeric
#'@import bit
#'@export
mac802 <- function(mac) {
    out <- strsplit(toupper(strsplit(mac, "([[:space:]])")[[1]][1]), ":")[[1]]
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
valid.mac802 <-function(x)
	if(length(x)!=6){
		return(FALSE)
	if(any(sapply(x,nchar)!=2))
		return(FALSE)
	if(gsub(mac802Regex,'',paste(collapse='',x))!='')
		return(FALSE)
	return(TRUE)
}

#' as.mac802
#' @param x a single value, coerced to numeric
#'@export
as.mac802 <-function(x){
	y<- as.numeric(x)
	if (valid.mac802(y)){
		class(y) <- "mac802"
    return(y)
	}
	stop(x)
}

#' is.mac802
#' @description a container for a valid RFC1918 IPv4 address
#' @param x a test object
#'@export
is.mac802 <- function(x){
	inherits(x,'mac802')	
}

#'print.mac802
#'@param x an 'mac802' object
#'@param ... passed to print.character
#'@export
print.mac802 <- function(x, ...) {
    cat(format(x),'\n',...)
}

#'format.mac802
#'@param x an 'mac802' object
#'@param ... ignored
#'@export
format.mac802 <- function(x, ...) {
    paste(collapse=':',x)
}

#''==.mac802
#'@param e1 an 'mac802' object
#'@param e2 an 'mac802' object
#'export
'==.mac802' <- function(e1,e2){
	if(format(e1)==format(e2))
		return(TRUE)
	return(FALSE)
}
