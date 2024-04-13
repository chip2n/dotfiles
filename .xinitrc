export FLYCTL_INSTALL=$HOME/.fly
export PATH=$FLYCTL_INSTALL/bin:$PATH

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
dunst &
udiskie --tray &

switch-keyboard-builtin

# Lenovo trackpoint acceleration and sensitivity
xinput --set-prop 12 'libinput Accel Speed' 1.0
#echo 180 > /sys/bus/serio/devices/serio2/sensitivity

# Disable trackpad on thinkpad
xinput --set-prop 11 'Device Enabled' 0

# Disable beep
xset -b

# Always be emacsin', like the lord intended
export EDITOR="emacsclient -n"

# ssh-agent
eval $(ssh-agent)

# environment variables
#source ~/env.sh

# desktop notifications
/usr/lib/notify-osd/notify-osd &

export _JAVA_AWT_WM_NONREPARENTING=1

# XMonad
exec xmonad

# BSPWM
#sxhkd &
#exec bspwm

# i3
#exec i3

# EXWM
#exec dbus-launch --exit-with-session emacs

# stumpwm
#exec sbcl --load ~/.stumpwm.d/startwm.lisp
