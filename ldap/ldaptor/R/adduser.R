

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
    
basednFromDomain <- function(domain){
	parts <- strsplit(domain,'\\.')[[1]]
	paste(collapse=',',sep='','dc=',parts)
}

krb_realm <- function(domain, admin_hosts = test_admin, subnet_layout = test_route, 
    base_ip = default_base_ip) {
    stopifnot(length(realm) == 1)
    stopifnot(all(strsplit(realm, "")[[1]] %in% valid_realm_chars))
    stopifnot(length(admin_hosts) > 0)
    stopifnot(all(do.call(c, strsplit(admin_hosts, "")) %in% letters))
    stopifnot(all(sapply(admin_hosts, nchar) <= 20))
    kdc <- length(admin_hosts)
    ldap <- min(2, kdc)
    hisec <- hisec_db[[min(length(hisec_db), length(admin_hosts))]]
    out <- list()
    out$realm <- toupper(domain)
    out$domain <- domain
    out$basedn <- basednFromDomain(domain)
    r_nets <- length(subnet_layout)
    a <- subnet_size(r_nets + length(admin_hosts))
    r <- sapply(routed_subnet_layout, subnet_size)
    names(s) <- names(subnet_layout)
    netlist <- sort(c(admin = a, s))
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

inc_ip <- function(ip,n=1) {
j<-1
while(j<=n){
    i <- 4
    overflow <- TRUE
    while (overflow) {
        ip[i] <- ip[i] + 1
        if (ip[i] < 256) 
            {overflow <- FALSE} else {
            ip[i] <- 0
            i <- i - 1
        }
    }
    j<-j+1
   }
    ip
    
}

default_shell = "/bin/bash"
default_base_ip = as.integer(c(10, 0, 0, 0))

addusertogroup <- function(user, group, basedn = default_basedn) {
    paste(sep = "", "dn: cn=", group, ",", basedn, "\n", "changeType: modify\n", 
        "add: memberUid", "memberUid: ", user, "\n", "\n")
}

adduser <- function(user, uid, gid, gecos = user, shell = default_shell, 
    domain) {
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

addusers <- function(users, startuid = 2000, usersgid = 100,
    domain) {
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

ldif_line <- function(key,value){
	paste(sep='',key,': ',value,'\n')
}

text_ip <- function(ip){
	paste(collapse='.',ip)
}

subnetmask <- function(bits){
	out<-rep(0,4)
	i<-1
	while(bits>8){
		bits<-bits-8
		out[i]<-255
		i<-i+1
	}
	octet <- 7
	while(bits>0){
		out[i]<-out[i]+2^octet
		octet<-octet-1
		bits<-bits-1
	}
	out
}

server_ldif <- function(server,domain){
	basedn <- basednFromDomain(domain)

dn: cn=config, ou=dhcp,dc=example,dc=com
cn: config
objectClass: top
objectClass: dhcpService
dhcpPrimaryDN:  cn=server,ou=dhcp,dc=example,dc=com
dhcpStatements: ddns-update-style none
dhcpStatements: get-lease-hostnames true
dhcpStatements: use-host-decl-names true
}

RFC1123 <- c(letters,LETTERS,'.','-',seq(0,9))

domain.class <- function(name){
	stopifnot(is.character(name))
	stopifnot(length(name)==1)
	stopifnot(length(setdiff(strsplit(name,'')[[1]],RFC1123))==0)
	out<- name
	class(out)<-c('domain.class',class(out))
	out
}

ldapquery <- function(pkey,basedn,skeylist=list(),kvlist=list()){
	stopifnot(is.ldapkv(key))
	stopifnot(is.basedn.class(basedn))
	stopifnot(all(sapply(c(skeylist,kvlist),is.ldapkv)))
	
	out<-list(pkey=pkey,basedn=basedn,skeylist=skeylist,kvlist=kvlist)
	class(out)<-'ldapquery'
	out
}

format.ldapquery <- function(x,...){
	dn <- sapply(x,format,sep='=')
	format(ldifkv('dn',paste(collapse=',',dn)))

}

is.ldapkv <- function(x){
	inherits(x,'ldapkv')
}
is.domain.class <- function(x){
	inherits(x,'domain.class')
}
is.basedn.class <- function(x){
	inherits(x,'basedn.class')
}

basedn.class <- function(domain){
	
	out <- lapply(strsplit(domain,'\\.')[[1]],ldifkv,x='dc')
	class(out) <- 'basedn.class'
	out
}

format.ldapdn <- function(x,...){
	dn <- sapply(x,format,sep='=')
	format(ldifkv('dn',paste(collapse=',',dn)))
}


# RFC2253 without '\' because Trumpette says '\' is silly
RFC2253special <- c( ",", "+", "\"", "<", ">", ";") 
RFC2253 <- c(RFC1123,RFC2253special)

ldapkv <- function(key,value){
	stopifnot(is.character(key))
	stopifnot(length(key)==1)
	stopifnot(length(setdiff(strsplit(key,'')[[1]],RFC2253))==0)
	stopifnot(is.character(value))
	stopifnot(length(value)==1)
	stopifnot(length(setdiff(strsplit(value,'')[[1]],RFC2253))==0)
	value<-list
	out<-c(x,...)
	class(out)<-'ldapkv'
	out
}

format.ldapkv <- function(x,sep=': ',...){
	gsub(paste(collapse='',RFC2253),'\\\\\\1',paste(collapse=sep,x))
}


subnet_ldif <- function(subnet,hosts,domain){
	net_ip <- subnet[1:4]
	router_ip <- inc_ip(net_ip)
	range_start <- inc_ip(router_ip,hosts)
	range_end <- inc_ip(range_start,hosts-2)
	netmask <- subnet[5]
	subnet <- subnetmask(netmask)
	basedn <- basednFromDomain(domain)
	list(c('dn',paste(sep='','cn=',net_ip,',cn=config,ou=dhcp,',basedn)),
		c('cn',text_ip(net_ip)),
		c('objectClass','top'),
		c('objectClass','dhcpSubnet'),
		c('objectClass','dhcpOptions'),
		c('dhcpNetMask',netmask),
		c('dhcpRange',do.call(paste,lapply(c(range_start,range_end),text_ip))),
		c('dhcpStatements','default-lease-time 14400'),
		c('dhcpStatements','max-lease-time 28800'),
		c('dhcpOption',paste('subnet-mask',text_ip(subnet))),
		c('dhcpOption',paste('routers',text_ip(router_ip))),
		c('dhcpOption',paste('domain-name-servers',text_ip(router_ip))),
		c('dhcpOption',paste(sep='','domain-name "',domain,'"'))
	)
}

export_networks_ldif <- function(networks) {
    paste(collapse = "\n", c(sapply(seq_along(networks), function(i) {
					name <- names(networks)[i]
        n <- networks[[i]]
        ip <- paste(collapse = ".", n[1:4])
        net <- paste(sep = "/", ip, n[5])
        paste(sep = "\t", net, name)
    }), ""))
}

export_hosts_ldif <- function(hosts) {
    paste(collapse = "\n", c(sapply(seq_along(hosts), function(i) {
        n <- hosts[[i]]
        ip <- paste(collapse = ".", n)
        paste(sep = "\t", ip, names(hosts[i]))
    }), ""))
}

