#!/bin/sh
systemctl stop slapd.service
slapd -d 5 -h ldapi:///