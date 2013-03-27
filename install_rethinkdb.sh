#!/bin/sh
apt-get update
apt-get -y install python-software-properties
add-apt-repository -y ppa:rethinkdb/ppa
apt-get update
apt-get -y install rethinkdb