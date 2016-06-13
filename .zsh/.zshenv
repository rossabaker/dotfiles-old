export EDITOR=emacsclient
export VISUAL=emacsclient
export ALTERNATE_EDITOR=emacs

zshenv_local=$ZDOTDIR/.zshenv.local
if [ -r $zshenv_local ]; then
    source $zshenv_local
fi

