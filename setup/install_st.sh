#!/bin/bash

set -e

DOTFILES=$HOME/dotfiles
TARGET=$HOME/st

git clone https://git.suckless.org/st $TARGET
cd $TARGET

ln -s $DOTFILES/st/config.h $TARGET/config.h
sudo make clean install
