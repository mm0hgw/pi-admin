#'/bin/sh

mkdir -p etc/dhcp
mkdir -p etc/default
mkdir -p etc/pam.d
cp -uR /etc/default/* etc/default
cp -uR /etc/pam.d/* etc/pam.d
cp -u /etc/dhcp/dhcpd.conf etc/dhcp/
cp -u /etc/dhcpcd.conf etc/
cp -u /etc/krb5.conf etc/
cp -u /etc/hosts etc/

mkdir -p srv
cp -uR /srv/tftp srv/tftp

mkdir -p usr/share/pam-configs
cp -u /usr/share/pam-configs/* usr/share/pam-configs/

git add *