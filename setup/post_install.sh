#!/bin/bash

USER="chip"

useradd -m $USER

read -p "Choose password: " PASSWORD1
read -p "Again: " PASSWORD2

if [ $PASSWORD1 = $PASSWORD2 ]
then
  echo "$USER:$PASSWORD1" | chpasswd
else
  echo "Passwords did not match"
fi

gpasswd -a $USER wheel
cp sudoers /etc/sudoers

pacman -S --noconfirm git
cd /home/$USER
git clone https://github.com/chip2n/dotfiles
