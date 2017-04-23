#!/usr/bin/env bash

for PID in `pgrep -x conky`; do
    kill ${PID} > /dev/null
done

conky -c /home/chip/.xmonad/conky_dzen | dzen2 -fn terminus-8 -x '1520' -y '4' -h '18' -w '1920' -ta 'r' -bg '#21242b' &
