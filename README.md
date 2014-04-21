dotfiles
========

arch linux configuration files

Installation
------------
Make symlinks from relevant directories. For standard bspwm functionality, symlink .config/bspwm, .config/sxhkd and scripts. Also, add the following line to /etc/profile:

    export PANEL_FIFO=/tmp/panel-fifo
    
The following packages are used by the bar:
* trayer-srg
* acpi
* xtitle
* conky

Also, the following packages are optional for the same functionality and look:
* stlarch_font
* dmenu
* gnome-themes-standard
* lxappearance

[Bar-aint-recursive](https://github.com/LemonBoy/bar) needs to be downloaded manually and compiled with the config.h present in bar/config.h.

Recommended GTK3 themes:
* Dorian Theme 3.10 (dorian-theme-3.10)   NOTE: Follow instructions to install the firefox theme.
