#!/usr/bin/env bash

setxkbmap chip
xmodmap /home/chip/.xmodmap

xset -b
xset r rate 200 25
