#!/bin/bash

public_dir=`dirname $0`/../public_repos
cd $public_dir
wget https://sourceforge.net/projects/cedet/files/cedet/cedet-1.1.tar.gz/download -O cedet-1.1.tar.gz
tar zxf cedet-1.1.tar.gz
cd cedet-1.1
make

