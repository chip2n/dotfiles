cat <<EOT >> /etc/X11/xorg.conf.d/10-monitor.conf
Section "Monitor"
    Identifier "DVI"
EndSection

Section "Monitor"
    Identifier "HDMI"
    Option "LeftOf" "DVI"
    Option "Primary" "true"
    Option "DPI" "96 x 96"
EndSection

Section "Screen"
  Identifier "Screen0"
  Device "nvidia"
  Monitor "HDMI"
EndSection

Section "Device"
    Identifier "Device0"
    Option "Monitor-HDMI-1" "HDMI"
    Option "Monitor-DVI-D-0" "DVI"
    Driver "nvidia"
EndSection
EOT
