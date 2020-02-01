xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/artwiz-fonts
xset +fp /home/chip/.fonts
xset fp rehash

dropbox &
compton &
feh --bg-scale /home/chip/dotfiles/gray.png
#skype -style GTK &
unclutter &
#xbacklight -set 100

export PATH=$HOME/.cabal/bin:$HOME/scripts:$HOME/local_scripts:$PATH:$HOME/.local/bin:$HOME/android/sdk/platform-tools:$HOME/android/sdk/emulator:$HOME/flutter/bin:$HOME/flutter/bin/cache/dart-sdk/bin:$HOME/.pub-cache/bin:$HOME/.cask/bin:$HOME/.gem/ruby/2.6.0/bin

switch-keyboard-builtin

# Lenovo trackpoint acceleration and sensitivity
xinput --set-prop 12 'libinput Accel Speed' 1.0
#echo 180 > /sys/bus/serio/devices/serio2/sensitivity

# Disable trackpad on thinkpad
xinput --set-prop 11 'Device Enabled' 0

# Disable beep
xset -b

# ssh-agent
eval $(ssh-agent)

# environment variables
#source ~/env.sh

# desktop notifications
/usr/lib/notify-osd/notify-osd &

export _JAVA_AWT_WM_NONREPARENTING=1

# Stumpwm
# exec stumpwm

# XMonad
exec xmonad

# BSPWM
#sxhkd &
#exec bspwm

# i3
#exec i3

# EXWM
#exec dbus-launch --exit-with-session emacs
