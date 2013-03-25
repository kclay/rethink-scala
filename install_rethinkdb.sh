#!/bin/sh
apt-get update
apt-get install python-software-properties
add-apt-repository ppa:rethinkdb/ppa
apt-get update
apt-get install rethinkdb