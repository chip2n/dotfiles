#!/bin/bash

DISK=/dev/sda
PART_BOOT=${DISK}1
PART_ROOT=${DISK}2
KEYMAP=sv_latin1
HOSTNAME=chipt0p-thinkpad

#ls /sys/firmware/efi/efivars
timedatectl set-ntp true
timedatectl status

# TODO Wipe whole hard drive
wipefs --all --force $PART_BOOT
wipefs --all --force $PART_ROOT

sed -e 's/\s*\([\+0-9a-zA-Z]*\).*/\1/' << EOF | fdisk $DISK
  g # wipe partition table
  n # create new partition
  1 # partiton 1
    # start at beginning of disk
  +1G
  t # set partition type
  1 # EFI System
  n # create new partition
  2 # partiton 2
    # start at beginning of disk
    # end at end of disk
  w
EOF
mkfs.fat -F32 $PART_BOOT
mkfs.ext4 $PART_ROOT

mount $PART_ROOT /mnt
mkdir /mnt/boot
mount $PART_BOOT /mnt/boot
pacstrap /mnt base base-devel
genfstab -U /mnt >> /mnt/etc/fstab

cp install2.sh /mnt/
arch-chroot /mnt /bin/bash -c "/install2.sh"
rm /mnt/install2.sh

umount -R /mnt
