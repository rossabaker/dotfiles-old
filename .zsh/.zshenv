export EDITOR="emacsclient -t -a=\\\"\\\""
export VISUAL=$EDITOR
export ALTERNATE_EDITOR="emacs -t"

zshenv_local=$ZDOTDIR/.zshenv.local
if [ -r $zshenv_local ]; then
    source $zshenv_local
fi

