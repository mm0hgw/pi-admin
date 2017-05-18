#!/bin/sh

systemctl stop slapd.service
rm -rf /etc/ldap/slapd.d/
mkdir /etc/ldap/slapd.d/
slaptest -f $1 -F /etc/ldap/slapd.d/
chown -R openldap:openldap /etc/ldap/slapd.d/
systemctl start slapd.service
systemctl status slapd.service -l

