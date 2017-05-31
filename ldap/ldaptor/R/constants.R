hexRegex <- "([0123456789ABCDEF])"

# 32-bit limit
ipv4.allOnes <- 2^33 - 1

#'RFC2253Regex
#'@description A regex to identify valid RFC2253 string characters
#'@export
RFC2253Regex <- "([-[:space:]ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\\.,+\\\"\\\\<=>;])"

#'RFC2253SpecialRegex
#'@description A regex to identify valid RFC2253 special characters
#'@export
RFC2253SpecialRegex <- "([,+\\\"\\\\<=>;])"

mac802OUI <- rbind(`Raspberry Pi` = c("B8", "27", "EB"))
