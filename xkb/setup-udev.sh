#!/usr/bin/env bash

set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

ln -sf /home/chip/dotfiles/xkb/udev.rules /etc/udev/rules.d/99-keyboard-planck.rules
ln -sf /home/chip/dotfiles/xkb/udev-trigger-planck.sh /usr/local/bin/planck-udev
ln -sf /home/chip/dotfiles/xkb/udev-trigger-builtin.sh /usr/local/bin/builtin-udev
ln -sf /home/chip/dotfiles/xkb/switch-keyboard-planck.sh /usr/local/bin/switch-keyboard-planck
ln -sf /home/chip/dotfiles/xkb/switch-keyboard-builtin.sh /usr/local/bin/switch-keyboard-builtin
ln -sf /home/chip/dotfiles/xkb/layout-planck /usr/share/X11/xkb/symbols/chip-planck
ln -sf /home/chip/dotfiles/xkb/layout-builtin /usr/share/X11/xkb/symbols/chip-builtin

udevadm control --reload-rules
