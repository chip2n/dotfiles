DISK=/dev/sda
BOOT_SIZE=512MiB
SWAP_SIZE=2GiB

parted $DISK -- mklabel gpt

parted $DISK -- mkpart primary $BOOT_SIZE -$SWAP_SIZE

parted $DISK -- mkpart primary linux-swap -$SWAP_SIZE 100%

parted $DISK -- mkpart ESP fat32 1MiB $BOOT_SIZE
parted $DISK -- set 3 boot on

mkfs.ext4 -L root ${DISK}1

mkswap -L swap ${DISK}2

mkfs.fat -F 32 -n boot ${DISK}3

mount /dev/disk/by-label/root /mnt

mkdir -p /mnt/boot
mount /dev/disk/by-label/boot /mnt/boot

swapon ${DISK}2
