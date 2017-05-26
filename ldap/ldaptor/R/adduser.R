

valid_realm_chars <- c(".", LETTERS)
valid_host_chars <- c(letters, seq(0, 9))
valid_domain_chars <- c(".", valid_host_chars)

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

hisec_db <- list(list(c("kadmin", "kdc1", "ldap", "nfs", "www", "ns1")), list(c("kadmin", 
    "kdc2", "ldap", "ns1"), c("kdc1", "nfs", "www", "ns2")), list(c("kadmin", "kdc2"), 
    c("kdc1", "ldap", "ns1"), c("nfs", "www", "ns2")), list(c("kadmin", "kdc2"), 
    c("kdc1", "ldap"), c("nfs", "ns1"), c("www", "ns2")))

basednFromDomain <- function(domain) {
    parts <- strsplit(domain, "\\.")[[1]]
    paste(collapse = ",", sep = "", "dc=", parts)
}

krb_realm <- function(domain, admin_hosts = test_admin, subnet_layout = test_route, 
    base_ip = default_base_ip) {
    stopifnot(length(admin_hosts) > 0)
    stopifnot(all(sapply(admin_hosts, valid.hostname.class)))
    stopifnot(all(sapply(domain, valid.domain.class)))
    stopifnot(all(sapply(admin_hosts, nchar) <= 50))
    kdc <- length(admin_hosts)
    ldap <- min(2, kdc)
    hisec <- hisec_db[[min(length(hisec_db), length(admin_hosts))]]
    out <- list()
    out$realm <- toupper(domain)
    out$domain <- domain.class(domain)
    out$basedn <- basedn.class(domain)
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
        if (net == "admin") 
            out$networks[[strsplit(out$domain, "\\.")[[1]][1]]] <- c(base_ip, netlist[i]) else out$networks[[net]] <- c(base_ip, netlist[i])
        host_ip <- inc_ip(base_ip)
        j <- 1
        while (j <= length(hostnames[[net]])) {
            hostname <- hostnames[[net]][j]
            if (net == "admin") {
                fqdn <- paste(sep = ".", hostname, out$domain)
                if (j <= kdc) {
                  servicenames <- do.call(c, c(hostname, hisec[j]))
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
            host_ip <- inc_ip(host_ip)
            j <- j + 1
        }
        base_ip <- next_subnet(base_ip, netlist[i])
        i <- i + 1
    }
    out
}

inc_ip <- function(ip, n = 1) {
    j <- 1
    while (j <= n) {
        i <- 4
        overflow <- TRUE
        while (overflow) {
            ip[i] <- ip[i] + 1
            if (ip[i] < 256) {
                overflow <- FALSE
            } else {
                ip[i] <- 0
                i <- i - 1
            }
        }
        j <- j + 1
    }
    ip
}

default_shell = "/bin/bash"
default_base_ip = as.integer(c(10, 0, 0, 0))

addusertogroup <- function(user, group, basedn = default_basedn) {
    paste(sep = "", "dn: cn=", group, ",", basedn, "\n", "changeType: modify\n", 
        "add: memberUid", "memberUid: ", user, "\n", "\n")
}

adduser <- function(user, uid, gid, gecos = user, shell = default_shell, domain) {
    basedn <- basednFromDomain(domain)
    paste(sep = "", "dn: uid=", user, ",ou=users,", basedn, "\n", "objectClass: top\n", 
        "objectClass: account\n", "objectClass: posixAccount\n", "cn: ", user, "\n", 
        "uid: ", user, "\n", "uidNumber: ", uid, "\n", "gidNumber: ", gid, "\n", 
        "homeDirectory: /home/", user, "\n", "loginShell: ", shell, "\n", "gecos: ", 
        gecos, "\n", "userPassword: {SASL}", user, "@", domain, "\n", "\n")
}

addgroup <- function(group, gid, domain) {
    basedn <- basednFromDomain(domain)
    paste(sep = "", "dn: cn=", group, ",ou=groups,", basedn, "\n", "objectClass: top\n", 
        "objectClass: posixGroup\n", "gidNumber: ", gid, "\n", "\n")
}

addusers <- function(users, startuid = 2000, usersgid = 100, domain) {
    sapply(seq_along(users), function(i) {
        uid = startuid - 1 + i
        adduser(users[i], uid, usersgid, domain)
    })
}

subnet_names <- function(netname, hosts) {
    paste(sep = "-", netname, seq(hosts))
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

export_networks_flatfile <- function(networks) {
    paste(collapse = "\n", c(sapply(seq_along(networks), function(i) {
        n <- networks[[i]]
        ip <- paste(collapse = ".", n[1:4])
        net <- paste(sep = "/", ip, n[5])
        paste(sep = "\t", net, names(networks[i]))
    }), ""))
}

export_hosts_flatfile <- function(hosts) {
    paste(collapse = "\n", c(sapply(seq_along(hosts), function(i) {
        n <- hosts[[i]]
        ip <- paste(collapse = ".", n)
        paste(sep = "\t", ip, names(hosts[i]))
    }), ""))
}

text_ip <- function(ip) {
    paste(collapse = ".", ip)
}

subnetmask <- function(bits) {
    out <- rep(0, 4)
    i <- 1
    while (bits > 8) {
        bits <- bits - 8
        out[i] <- 255
        i <- i + 1
    }
    octet <- 7
    while (bits > 0) {
        out[i] <- out[i] + 2^octet
        octet <- octet - 1
        bits <- bits - 1
    }
    out
}

server_ldif <- function(server, domain) {
}



ldapSubnet <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "dhcpSubnet"), 
        ldapkv("objectClass", "dhcpOptions"))

subnet_ldif <- function(subnet, domain,statements=list('default-lease-time 14400','max-lease-time 28800'),options=list()) {
    net_ip <- ipv4.class(subnet)
    router_ip <- net_ip+1
    netmask <- subnet[5]
    broadcast <- net_ip + (ipv4.class(rep(255, 4)) - subnetmask(netmask))
    pkey <- ldapkv("cn", text_ip(net_ip))
    skeylist <- list(ldapkv("cn", "config"), ldapkv("ou", "dhcp"))
    kvlist <-  c(ldapSubnet,ldapkv("dhcpNetMask", netmask), lapply(statements,ldapkv,key='dhcpStatements'))
      optionlist<- list( ldapkv("dhcpOption", paste("subnet-mask", format(subnetmask(netmask)))), ldapkv("dhcpOption", 
            paste("broadcast-address", text_ip(broadcast))), ldapkv("dhcpOption", 
            paste("routers", text_ip(router_ip))), ldapkv("dhcpOption", paste("domain-name-servers", 
            text_ip(router_ip))), ldapkv("dhcpOption", paste(sep = "", "domain-name \"", 
            domain, "\"")))
    ldapquery(pkey, domain, skeylist, c(kvlist,optionlist))
}

export_networks_ldif <- function(networks, domain) {
    lapply(seq_along(networks), function(i) {
        name <- names(networks)[i]
        n <- networks[[i]]
        subnet_ldif(n, domain)
    })
}

export_hosts_ldif <- function(hosts) {
    paste(collapse = "\n", c(sapply(seq_along(hosts), function(i) {
        n <- hosts[[i]]
        ip <- paste(collapse = ".", n)
        paste(sep = "\t", ip, names(hosts[i]))
    }), ""))
}

