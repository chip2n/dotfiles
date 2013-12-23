xset +fp /usr/share/fonts/local
xset +fp /home/chip/.fonts
xset fp rehash
setxkbmap se
dropboxd &
compton -c &
feh --bg-scale /home/chip/Dropbox/Wallpapers/ETrxee1.jpg

export PATH=$HOME/scripts:$HOME/local_scripts:$PATH

# XMonad
#exec xmonad

# BSPWM
sxhkd &
exec bspwm
