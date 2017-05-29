realm <- function(domain, admin_hosts = test_admin, subnet_layout = test_route, base_ip = default_base_ip) {
    stopifnot(all(sapply(admin_hosts, valid.hostname.class)))
    stopifnot(all(sapply(domain, valid.domain.class)))
    stopifnot(all(sapply(admin_hosts, nchar) <= 50))
    nAdmin <- length(admin_hosts)
    hisec <- max(1, min(length(hisec_db), nAdmin))
    out <- list()
    out$domain <- domain.class(domain)
    out$call <- match.call()
    r_nets <- length(subnet_layout)
    a <- subnet_size(r_nets + length(admin_hosts))
    r <- sapply(subnet_layout, subnet_size)
    names(r) <- names(subnet_layout)
    netlist <- sort(c(admin = a, r))
    out$networks <- list()
    out$hosts <- list()
    hostnames <- subnet_layout_names(subnet_layout)
    hostnames$admin <- c(admin_hosts, names(r))
    i <- 1
    while (i <= length(netlist)) {
        net <- names(netlist)[i]
        if (net == "admin") {
            out$networks[[strsplit(out$domain, "\\.")[[1]][1]]] <- c(base_ip, netlist[i])
            lapply(c("kadmin", "kdc", "ldap", "nfs", "www", "ns", "mail"), function(x) {
                key <- sapply(hisec_db[[hisec]], function(y) {
                  length(grep(x, y)) != 0
                })
                out[[x]] <- sapply(seq(hisec)[key], function(i) {
                  ipv4(base_ip) + i
                })
                
            })
        } else {
            out$networks[[net]] <- ipv4.subnet(c(base_ip, netlist[i]))
        }
        host_ip <- (base_ip + 1)
        j <- 1
        while (j <= length(hostnames[[net]])) {
            hostname <- hostnames[[net]][j]
            if (net == "admin") {
                fqdn <- paste(sep = ".", hostname, out$domain)
                if (j <= hisec) {
                  servicenames <- do.call(c, c(hostname, hisec_db[[hisec]][j]))
                  admin_dns <- paste(collapse = " ", servicenames)
                  fqdns <- paste(collapse = " ", sep = ".", servicenames, out$domain)
                  out$hosts[[paste(admin_dns, fqdns)]] <- host_ip
                } else {
                  fqdn <- paste(sep = ".", hostname, out$domain)
                  out$hosts[[paste(hostname, fqdn)]] <- host_ip
                }
            } else {
                fqdn <- paste(sep = ".", hostname, net, out$domain)
                out$hosts[[paste(hostname, fqdn)]] <- host_ip
            }
            host_ip <- host_ip+1
            j <- j + 1
        }
        base_ip <- next_subnet(base_ip, netlist[i])
        i <- i + 1
    }
    out$services <- lapply(c(" kadmin ", " kdc. ", " ns. ", " nfs ", " www ", " mail ", 
        " ldap "), function(service) {
        out$hosts[grep(service, names(out$hosts))]
    })
    names(out$services) <- c("kadmin", "kdc", "ns", "nfs", "www", "mail", "ldap")
    out
}

subnet_key <- 2^seq(24) - 2

is_subnet_layout <- function(s) {
    if (any(s%%1 != 0)) 
        return(FALSE)
    if (is.null(n <- names(s))) 
        return(FALSE)
    if (length(setdiff(do.call(c, strsplit(n, "")), valid_host_chars)) != 0) 
        return(FALSE)
    TRUE
}

test_admin <- c("grandpioverlord", "pioverlord")

test_route <- c(pispace = 8)

hisec_db <- list(list(c("kadmin", "kdc1", "ldap", "nfs", "www", "ns1", "mail")), 
    list(c("kadmin", "kdc2", "ldap", "ns2"), c("kdc1", "nfs", "www", "ns1", "mail")), 
    list(c("kadmin", "kdc2"), c("kdc1", "ldap", "ns1"), c("nfs", "www", "mail", "ns2")), 
    list(c("kadmin", "kdc2"), c("kdc1", "ldap"), c("nfs", "ns1"), c("www", "mail", 
        "ns2")))


default_shell = "/bin/bash"
default_base_ip = ipv4(10, 0, 0, 0)
subnet_names <- function(netname, hosts) {
    c(netname, paste(sep = "-", netname, seq(hosts - 1)))
}

subnet_layout_names <- function(s) {
    out <- lapply(seq_along(s), function(i) {
        subnet_names(names(s)[i], s[i])
    })
    names(out) <- names(s)
    out
}

subnet_size <- function(s) {
    out <- 31
    i <- 1
    while (s > subnet_key[i]) {
        out <- out - 1
        i <- i + 1
    }
    out
}

next_subnet <- function(ip, sn) {
    i <- 1
    while (sn > 8) {
        sn <- sn - 8
        i <- i + 1
    }
    ip[i] <- ip[i] + 2^(8 - sn)
    ip
}
