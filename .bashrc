# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto -h --group-directories-first'
alias sz='du -sk * | sort -n'
alias g='git'
alias c='clear'
alias top='htop'
alias weather='curl wttr.in/Gothenburg'

export LS_COLORS="di=1;34:"
export _JAVA_AWT_WM_NONREPARENTING=1

export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=100000
export HISTFILESIZE=100000

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Fix for using SBCL in emacs properly
unset SBCL_HOME

# Emacs vterm
vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Prompt
PS1="\[\e[00;34m\]\[\e[0m\]\W\[\e[00;34m\]> \[\e[0m\]"

# Emacs EAT
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"

# direnv
eval "$(direnv hook bash)"
