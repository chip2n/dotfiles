#!/usr/bin/env bash

for PID in `pgrep -x trayer`; do
    kill ${PID} > /dev/null &
done

trayer --edge top --align right --height 18 --width 4 --transparent true --alpha 0 --tint 0x21242b --distancefrom right --distance 320 &
