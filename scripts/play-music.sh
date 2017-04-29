#!/bin/sh

echo "http://${MPD_HOST}:${MPD_STREAM_PORT}" > ~/.config/mpd/playlists/chip-server-stream.m3u

mpd --kill
mpd
mpc clear
mpc --host 127.0.0.1 load chip-server-stream
mpc --host 127.0.0.1 play
ncmpcpp --host $MPD_HOST --port $MPD_CONTROL_PORT
