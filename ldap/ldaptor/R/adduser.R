

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

hisec_db <- list(list(c("kadmin", "kdc1", "ldap", "nfs", "www", "ns1", "mail")), 
    list(c("kadmin", "kdc2", "ldap", "ns2"), c("kdc1", "nfs", "www", "ns1", "mail")), 
    list(c("kadmin", "kdc2"), c("kdc1", "ldap", "ns1"), c("nfs", "www", "mail", "ns2")), 
    list(c("kadmin", "kdc2"), c("kdc1", "ldap"), c("nfs", "ns1"), c("www", "mail", 
        "ns2")))

realm <- function(domain, admin_hosts = test_admin, subnet_layout = test_route, base_ip = default_base_ip) {
    stopifnot(all(sapply(admin_hosts, valid.hostname.class)))
    stopifnot(all(sapply(domain, valid.domain.class)))
    stopifnot(all(sapply(admin_hosts, nchar) <= 50))
    nAdmin <- length(admin_hosts)
    hisec <- max(1, min(length(hisec_db), nAdmin))
    out <- list()
    out$realm <- toupper(domain)
    out$domain <- domain.class(domain)
    out$basedn <- basedn.class(domain)
    out$admin_hosts <- admin_hosts
    out$subnet_layout <- subnet_layout
    out$base_ip <- base_ip
    r_nets <- length(subnet_layout)
    a <- subnet_size(r_nets + length(admin_hosts))
    r <- sapply(subnet_layout, subnet_size)
    names(r) <- names(subnet_layout)
    netlist <- sort(c(admin = a, r))
    out$networks <- list()
    out$hosts <- list()
    out$hostnames <- subnet_layout_names(subnet_layout)
    out$hostnames$admin <- c(admin_hosts, names(r))
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
                  ipv4.class(base_ip) + i
                })
                
            })
        } else {
            out$networks[[net]] <- c(base_ip, netlist[i])
        }
        host_ip <- (base_ip + 1)
        j <- 1
        while (j <= length(out$hostnames[[net]])) {
            hostname <- out$hostnames[[net]][j]
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
            host_ip <- inc_ip(host_ip)
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

default_shell = "/bin/bash"
default_base_ip = ipv4.class(10, 0, 0, 0)

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
    ipv4.class(out)
}

ldapDhcpList <- function(x, key = "dhcpStatements") {
    if (length(x) == 1) {
        x <- strsplit(x, "\n")
    }
    x <- x[x != ""]
    x <- grep("^#", x, value = TRUE, invert = TRUE)
    x <- gsub(";$", "", x)
    sapply(x, ldapkv, key = key)
}

ldapOu <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "organizationalUnit"))
# ou

ldapUser <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "account"), 
    ldapkv("objectClass", "posixAccount"))
# cn uid uidNumber gidNumber homeDirectory loginShell gecos userPassword:
# {SASL}uid@REALM
ldapGroup <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "posixGroup"))
# cn gidNumber memberUid
ldapHost <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "ipHost"), ldapkv("objectClass", 
    "device"))
# cn ipHostNumber
ldapNetwork <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "ipNetwork"))
# cn ipNetworkNumber ipNetmaskNumber
ldapEther <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "ieee802device"), 
    ldapkv("objectClass", "device"))
# cn macAddress
ldapNetgroup <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "nisNetgroup"), 
    ldapkv("objectClass", "device"))
# cn nisNetgroupTriple memberNisNetgroup

ldapDhcpServer <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "dhcpServer"))
# cn dhcpServiceDN
ldapDhcpSubnet <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "dhcpSubnet"), 
    ldapkv("objectClass", "dhcpOptions"))
# cn dhcpNetMask dhcpStatements dhcpOption
ldapDhcpHost <- list(ldapkv("objectClass", "top"), ldapkv("objectClass", "dhcpHost"))
# cn dhcpHWAddress dhcpStatements

ldapDhcpServerDef <- function(name) ldapkvlist(ldapkv("cn", name), ldapkv("ou", "dhcp"))

exportDhcpServers.ldif <- function(realm) {
    out<-lapply(names(realm$networks), function(network) {
        server <- ldapDhcpServerDef(network)
        servicedn <- ldapkv("dhcpServiceDN", paste(collapse = ",", sapply(c(server, 
            realm$basedn), format, collapse = "=")))
        kvlist <- c(ldapDhcpServer, list(servicedn))
        pkey <- server[[1]]
        skeylist <- server[2]
        out <- ldapquerylist(ldapquery(pkey, realm$basedn, skeylist, kvlist))
        subnet <- realm$networks[[network]]
        netip <- ipv4.class(subnet)
        router <- netip + 1
        netmask <- subnet[5]
        broadcast <- ipv4.class(as.vector(netip) + rep(255, 4) - as.vector(subnetmask(netmask)))
        statements <- list("default-lease-time 14400", "max-lease-time 28800")
        pkey <- ldapkv("cn", format(netip))
        skeylist <- server
        kvlist <- c(ldapDhcpSubnet, list(ldapkv("dhcpNetMask", netmask)), lapply(statements, 
            ldapkv, key = "dhcpStatements"), list(ldapkv("dhcpOption", paste("subnet-mask", 
            format(subnetmask(netmask)))), ldapkv("dhcpOption", paste("broadcast-address", 
            format(broadcast))), ldapkv("dhcpOption", paste("routers", format(router))), 
            ldapkv("dhcpOption", paste(collapse = " ", c("domain-name-servers", sapply(out$services$ns, 
                format)))), ldapkv("dhcpOption", paste(sep = "", "domain-name \"", 
                realm$domain, "\""))))
        out <- ldapquerylist(c(out, list(ldapquery(pkey, realm$basedn, skeylist, 
            kvlist))))
        hostip <- router
        hosts <- ldapquerylist()
        while (hostip < broadcast) {
            if (sum(hostindex <- sapply(realm$hosts, "==", hostip)) > 0) {
                
                host <- names(realm$hosts)[hostindex]
                cns <- strsplit(host, " ")[[1]]
                cnlist <- lapply(cns, ldapkv, key = "cn")
                pkey <- cnlist[[1]]
                kvlist <- c(ldapDhcpHost, list(ldapkv("dhcpStatements", paste("fixed-address", 
                  format(hostip)))), cnlist[-1])
                hosts <- hosts + ldapquery(pkey, realm$basedn, skeylist, kvlist)
            }
            hostip <- hostip + 1
        }
        print(summary(out))
        print(summary(hosts))
        out <- out + hosts
        out
    })
    names(out) <- names(realm$networks)
    out
}
