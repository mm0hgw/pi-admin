exportNetworks.flatfile <- function(realm) {
    paste(collapse = "\n", c(sapply(seq_along(realm$networks), function(i) {
        n <- realm$networks[[i]]
        ip <- paste(collapse = ".", n[1:4])
        net <- paste(sep = "/", ip, n[5])
        paste(sep = "\t", net, names(realm$networks[i]))
    }), ""))
}

exportHosts.flatfile <- function(realm) {
    paste(collapse = "\n", c(sapply(seq_along(realm$hosts), function(i) {
        n <- realm$hosts[[i]]
        ip <- paste(collapse = ".", n)
        paste(sep = "\t", ip, names(realm$hosts[i]))
    }), ""))
}
