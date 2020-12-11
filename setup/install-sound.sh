#/usr/bin/env bash

pacman -Syu pro-audio pulseaudio-jack
usermod chip -aG realtime
