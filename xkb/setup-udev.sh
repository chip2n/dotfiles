#!/usr/bin/env bash

set -e

ln -sf /home/chip/dotfiles/xkb/udev.rules /etc/udev/rules.d/99-keyboard-planck.rules
ln -sf /home/chip/dotfiles/xkb/udev-trigger-planck.sh /usr/local/bin/planck-udev
ln -sf /home/chip/dotfiles/xkb/switch-keyboard-planck.sh /usr/local/bin/switch-keyboard-planck

udevadm control --reload-rules
