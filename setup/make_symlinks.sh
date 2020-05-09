#!/bin/bash

set -e

ln -s /home/chip/dotfiles/.xinitrc /home/chip/.xinitrc
ln -s /home/chip/dotfiles/.xmodmap /home/chip/.xmodmap
ln -s /home/chip/dotfiles/.xmonad /home/chip/.xmonad
ln -s /home/chip/dotfiles/.colors /home/chip/.colors
ln -s /home/chip/dotfiles/.urxvt /home/chip/.urxvt
ln -s /home/chip/dotfiles/.compton.conf /home/chip/.compton.conf
ln -s /home/chip/dotfiles/.Xdefaults /home/chip/.Xdefaults
ln -s /home/chip/dotfiles/.zshrc /home/chip/.zshrc
ln -s /home/chip/dotfiles/.bashrc /home/chip/.bashrc
ln -s /home/chip/dotfiles/.config /home/chip/.config
ln -s /home/chip/dotfiles/scripts /home/chip/scripts
ln -s /home/chip/dotfiles/.emacs.d /home/chip/.emacs.d
ln -s /home/chip/dotfiles/.nethackrc /home/chip/.nethackrc
ln -s /home/chip/dotfiles/.mbsyncrc /home/chip/.mbsyncrc
ln -s /home/chip/dotfiles/.sbclrc /home/chip/.sbclrc
ln -s /home/chip/dotfiles/audio /home/chip/audio
ln -s /home/chip/Dropbox/elfeed /home/chip/.elfeed
ln -s /home/chip/dotfiles/xkb/layout-planck /usr/share/X11/xkb/symbols/chip-planck
ln -s /home/chip/dotfiles/xkb/layout-builtin /usr/share/X11/xkb/symbols/chip-builtin
ln -s /home/chip/dotfiles/.ideavimrc /home/chip/.ideavimrc

systemctl enable --user emacsd.service

/home/chip/dotfiles/xkb/setup-udev.sh
