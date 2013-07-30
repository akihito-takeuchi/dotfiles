#!/bin/bash

public_dir=`dirname $0`/../public_repos
cd $public_dir
wget http://www.emacswiki.org/emacs/download/auto-install.el
emacs --batch -Q -f batch-byte-compile auto-install.el
