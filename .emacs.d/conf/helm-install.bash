#!/bin/bash

public_dir=`dirname $0`/../public_repos
cd $public_dir
git clone https://github.com/emacs-helm/helm
cd helm
git checkout v1.4.9
