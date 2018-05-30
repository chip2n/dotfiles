#!/bin/bash

IS_INTEL_CPU=true

ln -sf /usr/share/zoneinfo/Europe/Stockholm /etc/localtime
hwclock --systohc
sed -i.old '1s;^;en_US.UTF-8 UTF-8\n\n;' /etc/locale.gen
echo "KEYMAP=$KEYMAP" >> /etc/vconsole.conf
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

bootctl --path=/boot install

if $IS_INTEL_CPU; then
  pacman -S --noconfirm intel-ucode
  echo "title   Arch Linux" >> /boot/loader/entries/entry.conf
  echo "linux   /vmlinuz-linux" >> /boot/loader/entries/entry.conf
  echo "initrd  /intel-ucode.img" >> /boot/loader/entries/entry.conf
  echo "initrd  /initramfs-linux.img" >> /boot/loader/entries/entry.conf
  echo "options root=/dev/sda2 rw" >> /boot/loader/entries/entry.conf
fi
