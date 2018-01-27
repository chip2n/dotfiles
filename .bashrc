#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto -h --group-directories-first'
alias sz='du -sk * | sort -n'
alias g='git'
alias c='cabal'
alias cl='clear'
alias woman,='sudo'
alias feh='feh --scale-down'
alias top='htop'
alias vim='nvim'
alias web='qutebrowser --backend webengine'
alias wifi='sudo wifi-menu'
PS1='\[\e[1;30m\]>>  \[\e[m\]'

export LS_COLORS="di=1;34:"
export _JAVA_AWT_WM_NONREPARENTING=1

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
