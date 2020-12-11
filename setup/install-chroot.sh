#!/bin/bash

set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

# set timezone
ln -sf /usr/share/zoneinfo/Europe/Stockholm /etc/localtime
hwclock --systohc

# localization
echo "LANG=en_US.UTF-8" >> /etc/locale.conf
sed -i.old '1s;^;en_US.UTF-8 UTF-8\n\n;' /etc/locale.gen
locale-gen
echo $HOSTNAME >> /etc/hostname
echo "127.0.0.1    localhost" >> /etc/hosts
echo "::1          localhost" >> /etc/hosts
echo "127.0.1.1    $HOSTNAME.localdomain    $HOSTNAME" >> /etc/hosts

pacman -Syuu --noconfirm
pacman -S --noconfirm iw wpa_supplicant dialog

mkinitcpio -p linux

read -p "Choose root password: " PASSWORD1
read -p "Again: " PASSWORD2

if [ $PASSWORD1 = $PASSWORD2 ]
then
  echo "root:$PASSWORD1" | chpasswd
else
  echo "Passwords did not match"
fi

if $INSTALL_BOOT_LOADER; then
  bootctl --path=/boot install
fi
