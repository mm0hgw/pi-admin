
#'@method valid poolDef
valid.poolDef <- function(x){
	if(!identical(typeof(x),'list'))
		return(FALSE)
	if(is.null(x$subnet) || !is.ipv4.subnet(subnet))
		return(FALSE)
	if(is.null(x$start) || !is.ipv4(start))
		return(FALSE)
	if(is.null(x$end) || !is.ipv4(end))
		return(FALSE)
	if(start <= x$subnet$ip)
		return(FALSE)
	if(end < start)
		return(FALSE)
	if(broadcast(x$subnet) <= end)
		return(FALSE)

	return(TRUE)	
}

#'poolDef
#'@export
poolDef <- function(subnet,start,end,...){
	as.poolDef(as.list(match.call())[-1])
}

#'@method as poolDef
as.poolDef <- function(object,...){
	UseMethod('as.poolDef',object)
}

#'@method as.poolDef default
as.poolDef <- function(object,...){
	if(!valid.poolDef(object))
		stop()
	class(object) <- 'poolDef'
	object
}
