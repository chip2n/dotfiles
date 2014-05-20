#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto -l -h --group-directories-first'
alias sz='du -sk * | sort -n'
alias cabal='/home/chip/.cabal/bin/cabal'
alias g='git'
alias c='cabal'
alias cl='clear'
alias woman,='sudo'
alias feh='feh --scale-down'
alias vimr='gvim /home/chip/.vim/vimrc'
#PS1='[\u@\h \W]\$ '
PS1='»  '
