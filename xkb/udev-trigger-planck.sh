#!/usr/bin/env bash

HOME=/home/chip
XAUTHORITY=$HOME/.Xauthority
export XAUTHORITY HOME
DISPLAY=:0 ; export DISPLAY;

# Path to lock file
lock="/tmp/keyboard.lock"

# Lock the file (other atomic alternatives would be "ln" or "mkdir")
exec 9>"$lock"
if ! flock -n 9; then
        notify-send -t 5000 "Keyboard script is already running."
        exit 1
fi

/usr/bin/su chip -c "/usr/local/bin/switch-keyboard-planck;" &

# The lock file will be unlocked when the script ends
echo '' > /tmp/keyboard.lock &
unset DISPLAY
