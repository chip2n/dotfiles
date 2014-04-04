xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/artwiz-fonts
xset +fp /home/chip/.fonts
xset fp rehash
setxkbmap se
dropboxd &
compton -c -C -r 4 -l 1 -t 1 -o 0.3 --inactive-dim 0.3 &
feh --bg-scale /home/chip/Dropbox/Wallpapers/grey.png
skype &

export PATH=$HOME/.cabal/bin:$HOME/scripts:$HOME/local_scripts:$PATH

# XMonad
#exec xmonad

# BSPWM
sxhkd &
exec bspwm
