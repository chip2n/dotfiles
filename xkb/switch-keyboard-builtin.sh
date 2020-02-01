#!/usr/bin/env bash

# for some reason, the keymap is not properly setup unless we sleep for
# a bit before activating it
sleep 1

setxkbmap chip-builtin
xmodmap /home/chip/.xmodmap

xset -b
xset r rate 200 25
