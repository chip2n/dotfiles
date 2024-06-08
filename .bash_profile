export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.cabal/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.pub-cache/bin:$PATH
export PATH=$HOME/.cask/bin:$PATH
export PATH=$HOME/.gem/ruby/2.7.0/bin:$PATH
export PATH=$HOME/.gem/ruby/3.0.0/bin:$PATH
export PATH=$HOME/Android/Sdk/platform-tools:$PATH
export PATH=$HOME/Android/Sdk/tools/bin:$PATH
export PATH=$HOME/Android/Sdk/emulator:$PATH
export PATH=$HOME/scripts:$PATH
export PATH=$HOME/flutter/bin:$PATH

GUIX_PROFILE="/home/chip/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

GUIX_PROFILE="/home/chip/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"

[[ -f ~/.bashrc ]] && . ~/.bashrc
