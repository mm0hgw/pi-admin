
exportDhcpServers.ldif <- function(realm) {
    out <- lapply(names(realm$networks), function(network) {
        server <- ldapDhcpServerDef(network)
        servicedn <- ldapkv("dhcpServiceDN", paste(collapse = ",", sapply(c(server, 
            realm$basedn), format, collapse = "=")))
        kvlist <- c(ldapDhcpServer, list(servicedn))
        pkey <- server[[1]]
        skeylist <- server[2]
        out <- ldapquerylist(ldapquery(pkey, realm$basedn, skeylist, kvlist))
        subnet <- realm$networks[[network]]
        netip <- ipv4(subnet)
        router <- netip + 1
        netmask <- subnet[5]
        broadcast <- ipv4(as.vector(netip) + rep(255, 4) - as.vector(subnetmask(netmask)))
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
        out <- out + hosts
        out
    })
    names(out) <- names(realm$networks)
    out
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
