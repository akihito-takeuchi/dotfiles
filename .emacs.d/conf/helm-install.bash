#!/bin/bash

public_dir=`dirname $0`/../public_repos
cd $public_dir
git clone https://github.com/emacs-helm/helm
git clone https://github.com/jwiegley/emacs-async
cd helm
git checkout v1.6.8
make

