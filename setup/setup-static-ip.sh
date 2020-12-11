#!/usr/bin/env bash

NETWORK="enp33s0"
STATIC_IP="192.168.10.2"

echo "[Match]" >> /etc/systemd/network/20-wired.network
echo "Name=${NETWORK}" >> /etc/systemd/network/20-wired.network
echo "" >> /etc/systemd/network/20-wired.network
echo "[Network]" >> /etc/systemd/network/20-wired.network
echo "Address=${STATIC_IP}/24" >> /etc/systemd/network/20-wired.network
echo "Gateway=192.168.10.1" >> /etc/systemd/network/20-wired.network
echo "DNS=192.168.10.1" >> /etc/systemd/network/20-wired.network
