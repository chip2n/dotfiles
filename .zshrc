# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/chip/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
#

autoload -U colors
colors
autoload -U promptinit
PROMPT='»  '

alias ls='ls --color=auto -l -h --group-directories-first'
alias sz='du -sk * | sort -n'
alias cabal='/home/chip/.cabal/bin/cabal'
alias g='git'
alias c='cabal'
alias cl='clear'
alias woman,='sudo'
alias feh='feh --scale-down'
alias vimr='gvim /home/chip/.vim/vimrc'
