#'/bin/sh

mkdir -p etc/dhcp
mkdir -p etc/default
mkdir -p etc/pam.d
mkdir -p etc/krb5kdc
cp -uR /etc/default/* etc/default
cp -uR /etc/pam.d/* etc/pam.d
cp -u /etc/dhcp/dhcpd.conf etc/dhcp/
cp -u /etc/dhcpcd.conf etc/
cp -u /etc/krb5.conf etc/
cp -u /etc/hosts etc/
sudo cp -u /etc/krb5kdc/kdc.conf etc/krb5kdc/
sudo chmod 777 etc/krb5kdc/kdc.conf

mkdir -p srv
cp -uR /srv/tftp srv/tftp

mkdir -p usr/share/pam-configs
cp -u /usr/share/pam-configs/* usr/share/pam-configs/

git add *