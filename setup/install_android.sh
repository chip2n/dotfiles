#!/bin/bash

sudo pacman -S android-udev

cower -d android-sdk
cower -d android-platform
cower -d android-sdk-platform-tools
cower -d android-sdk-build-tools

cd android-sdk
makepkg -ci
cd ..
#rm -r android-sdk
