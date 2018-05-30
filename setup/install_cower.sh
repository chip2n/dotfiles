#!/bin/bash

gpg --recv-keys --keyserver hkp://pgp.mit.edu 1EB2638FF56C0C53          
mkdir cower
cd cower
wget -O PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=cower
makepkg -ci
cd ..
