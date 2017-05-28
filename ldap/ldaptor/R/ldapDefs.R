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
