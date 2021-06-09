#!/bin/bash

set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

ln -sf /home/chip/dotfiles/.xinitrc /home/chip/.xinitrc
ln -sf /home/chip/dotfiles/.xmodmap /home/chip/.xmodmap
ln -sf /home/chip/dotfiles/.xmonad /home/chip/.xmonad
ln -sf /home/chip/dotfiles/.colors /home/chip/.colors
ln -sf /home/chip/dotfiles/.urxvt /home/chip/.urxvt
ln -sf /home/chip/dotfiles/.compton.conf /home/chip/.compton.conf
ln -sf /home/chip/dotfiles/.Xdefaults /home/chip/.Xdefaults
ln -sf /home/chip/dotfiles/.zshrc /home/chip/.zshrc
ln -sf /home/chip/dotfiles/.bashrc /home/chip/.bashrc
ln -sf /home/chip/dotfiles/.config /home/chip/.config
ln -sf /home/chip/dotfiles/scripts /home/chip/scripts
ln -sf /home/chip/dotfiles/.emacs.d /home/chip/.emacs.d
ln -sf /home/chip/dotfiles/.nethackrc /home/chip/.nethackrc
ln -sf /home/chip/dotfiles/.mbsyncrc /home/chip/.mbsyncrc
ln -sf /home/chip/dotfiles/.sbclrc /home/chip/.sbclrc
ln -sf /home/chip/dotfiles/audio /home/chip/audio
ln -sf /home/chip/Dropbox/elfeed /home/chip/.elfeed
ln -sf /home/chip/dotfiles/.ideavimrc /home/chip/.ideavimrc
ln -sf /home/chip/dotfiles/.asoundrc /home/chip/.asoundrc
ln -sf /home/chip/dotfiles/.slynkrc /home/chip/.slynkrc
