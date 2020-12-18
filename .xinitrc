export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.pub-cache/bin:$PATH
export PATH=$HOME/.cask/bin:$PATH
export PATH=$HOME/.gem/ruby/2.7.0/bin:$PATH
export PATH=$HOME/Android/Sdk/platform-tools:$PATH
export PATH=$HOME/Android/Sdk/emulator:$PATH
export PATH=$HOME/flutter/bin:$PATH
export PATH=$HOME/flutter/bin/cache/dart-sdk/bin:$PATH
export PATH=$HOME/scripts:$PATH
export PATH=$HOME/local_scripts:$PATH

# build rust crates in a single shared directory
export CARGO_TARGET_DIR="$HOME/.cargo/shared"

xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/artwiz-fonts
xset +fp /home/chip/.fonts
xset fp rehash

tresorit --hidden &
#compton &
feh --bg-scale /home/chip/dotfiles/gray.png
#skype -style GTK &
unclutter &
#xbacklight -set 100

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
