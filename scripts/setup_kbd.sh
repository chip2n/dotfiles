#!/bin/bash

setxkbmap chip
xmodmap /home/chip/.xmodmap
#xcape -e 'Control_L=Escape;'

xset -b
xset r rate 200 25
