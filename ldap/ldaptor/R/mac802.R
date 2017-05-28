
mac802Regex <- '([0123456789ABCDEF])'

mac802 <- function(mac){
	out<-strsplit(toupper(strsplit(mac,'[:space:]')[[1]][1]),':')[[1]]
	if(!valid.mac802(out)){
		print(mac)
		print(out)
		stop()
	}
	class(out)<-'mac802'
	out
}

valid.mac802 <- function(x){
	if(length(x)!=6)
		return(FALSE)
	if(any(sapply(x,nchar)!=2))	
	return(FALSE)
	if(!all(sapply(x,gsub,pattern=mac802Regex,replacement='')==''))	
	return(FALSE)
	return(TRUE)
}
