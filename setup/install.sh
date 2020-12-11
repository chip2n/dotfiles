#!/bin/bash

set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

# If dual booting, it's recommended to let windows create the EFI partition.
# In this case, set PART_BOOT to that partition and INSTALL_BOOT_LOADER to false.

DISK=/dev/sda
PART_BOOT=${DISK}1
PART_ROOT=${DISK}2
INSTALL_BOOT_LOADER=true

read -p "Hostname: " HOSTNAME

function partition_with_boot {
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
}

function partition_without_boot {
sed -e 's/\s*\([\+0-9a-zA-Z]*\).*/\1/' << EOF | fdisk $DISK
  g # wipe partition table
  n # create new partition
  1 # partiton 1
    # start at beginning of disk
    # end at end of disk
  w
EOF
}

echo "Partitioning disk..."
if $INSTALL_BOOT_LOADER; then
  partition_with_boot
  mkfs.fat -F32 $PART_BOOT
else
  partition_without_boot
fi

# setup luks
cryptsetup luksFormat -s 512 $PART_ROOT
cryptsetup open $PART_ROOT enc
mkfs.ext4 /dev/mapper/enc

# mount filesystem
mount /dev/mapper/enc /mnt
mkdir /mnt/boot
mount $PART_BOOT /mnt/boot

# install essential packages
pacstrap /mnt base base-devel linux linux-firmware

# configure the system
genfstab -U /mnt >> /mnt/etc/fstab
cp install-chroot.sh /mnt/
cp mkinitcpio.conf /mnt/etc/mkinitcpio.conf
arch-chroot /mnt env HOSTNAME="$HOSTNAME" INSTALL_BOOT_LOADER="$INSTALL_BOOT_LOADER" /bin/bash -c "/install-chroot.sh"
rm /mnt/install-chroot.sh

# clean up
umount -R /mnt
cryptsetup close enc
