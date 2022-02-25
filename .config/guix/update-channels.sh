#!/usr/bin/env sh

# Run this script to update all the Guix channels.
# Each channel will be locked to a specific commit inside channels.scm for reproducible builds.

guix pull --channels=$HOME/.config/guix/my-channels.scm
guix describe --format=channels > $HOME/.config/guix/channels.scm
