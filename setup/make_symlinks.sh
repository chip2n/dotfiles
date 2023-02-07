#!/usr/bin/env bash

set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

DOTFILES=$HOME/dev/dotfiles

ln -sf $DOTFILES/.asoundrc $HOME/.asoundrc
ln -sf $DOTFILES/.bashrc $HOME/.bashrc
ln -sf $DOTFILES/.colors $HOME/.colors
ln -sf $DOTFILES/.compton.conf $HOME/.compton.conf
ln -sf $DOTFILES/.config $HOME/.config
ln -sf $DOTFILES/.emacs.d $HOME/.emacs.d
ln -sf $DOTFILES/.ideavimrc $HOME/.ideavimrc
ln -sf $DOTFILES/.nethackrc $HOME/.nethackrc
ln -sf $DOTFILES/.sbclrc $HOME/.sbclrc
ln -sf $DOTFILES/.slynkrc $HOME/.slynkrc
ln -sf $DOTFILES/.urxvt $HOME/.urxvt
ln -sf $DOTFILES/.Xdefaults $HOME/.Xdefaults
ln -sf $DOTFILES/.xinitrc $HOME/.xinitrc
ln -sf $DOTFILES/.xmodmap $HOME/.xmodmap
ln -sf $DOTFILES/.xmonad $HOME/.xmonad
ln -sf $DOTFILES/.zshrc $HOME/.zshrc
ln -sf $DOTFILES/audio $HOME/audio
ln -sf $DOTFILES/git_template $HOME/.git_template
ln -sf $DOTFILES/scripts $HOME/scripts

ln -sf $DOTFILES/.mbsyncrc $HOME/.mbsyncrc
ln -sf $DOTFILES/.notmuch-config $HOME/.notmuch-config

ln -sf $HOME/storage/elfeed $HOME/.elfeed
