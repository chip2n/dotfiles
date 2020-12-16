#/usr/bin/env bash

mkdir -p /etc/systemd/system/getty@tty1.service.d
cat << EOT > /etc/systemd/system/getty@tty1.service.d/override.conf
[Service]
ExecStart=
ExecStart=-/usr/bin/agetty --autologin chip --noclear %I \$TERM
EOT
