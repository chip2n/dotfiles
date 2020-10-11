#!/usr/bin/env bash

sudo pacman -S --needed android-tools

cd /tmp
auracle clone scrcpy
cd scrcpy
makepkg -ci
