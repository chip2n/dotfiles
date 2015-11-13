#!/usr/bin/env bash

for PID in `pgrep -x conky`; do
    kill ${PID} > /dev/null
done

conky -c /home/chip/.xmonad/conky_dzen | dzen2 -fn terminus-8 -x '910' -y '0' -h '18' -w '960' -ta 'r' -bg '#1b1d1e' &
