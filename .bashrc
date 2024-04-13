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
alias weather='curl wttr.in/Gothenburg'
PS1='\[\e[1;30m\]>>  \[\e[m\]'

export LS_COLORS="di=1;34:"
export _JAVA_AWT_WM_NONREPARENTING=1

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Fix for using SBCL in emacs properly
unset SBCL_HOME

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/chip/google-cloud-sdk/path.bash.inc' ]; then source '/home/chip/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/chip/google-cloud-sdk/completion.bash.inc' ]; then source '/home/chip/google-cloud-sdk/completion.bash.inc'; fi

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

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

# Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi

eval "$(direnv hook bash)"
