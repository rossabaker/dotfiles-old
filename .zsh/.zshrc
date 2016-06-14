autoload -U compinit && zmodload -i zsh/complist
autoload -U colors && colors

typeset -U fpath
fpath+=(~/.zsh/functions)

autoload -U promptinit && promptinit && prompt ross

HISTFILE=~/.zhistory
HISTSIZE=20000
SAVEHIST=20000
setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history

setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

if [ -n "$INSIDE_EMACS" ]; then
    # Help emacs track current directory
    chpwd() { print -P "\033AnSiTc %d" }
    print -P "\033AnSiTu %n"
    print -P "\033AnSiTc %d"
fi

alias ls='ls -Gp'
alias grep="grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg.svn}"

zshrc_local=$ZDOTDIR/.zshrc.local
if [ -r $zshrc_local ]; then
    source $zshrc_local
fi
