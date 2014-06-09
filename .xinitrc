xset +fp /usr/share/fonts/local
xset +fp /usr/share/fonts/artwiz-fonts
xset +fp /home/chip/.fonts
xset fp rehash
setxkbmap se
dropboxd &
# compton -c -C -r 4 -l 1 -t 1 -o 0.3 --inactive-dim 0.3 &
feh --bg-scale /home/chip/Dropbox/wallpapers/grey.png
skype &
unclutter &
xbacklight -set 100

export PATH=$HOME/.cabal/bin:$HOME/scripts:$HOME/local_scripts:$PATH

export CLASSPATH=/usr/lib/jvm/java-7-openjdk/lib/tools.jar

# XMonad
#exec xmonad

# BSPWM
sxhkd &
exec bspwm
