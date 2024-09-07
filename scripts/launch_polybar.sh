#!/usr/bin/env bash

MODE=${1:-"single"}
echo $MODE

# terminate already running bar instances
killall -q polybar

# wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# launch bar
case $MODE in
    "single")
        polybar single_xmonad &;;
    "multi")
        polybar multi_main_xmonad &
        polybar multi_support_xmonad &;;
esac
