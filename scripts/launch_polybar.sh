#!/bin/bash

# terminate already running bar instances
killall -q polybar

# wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# launch bar
#polybar multi_main_bspwm &
#polybar multi_support_bspwm &

polybar single_xmonad &

#polybar single_bspwm &
