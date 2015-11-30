#!/bin/sh

DOTDIR=$(dirname $(readlink -f $0))

ln -sv $DOTDIR/git/gitconfig ~/.gitconfig
ln -sv $DOTDIR/zsh/zshrc ~/.zshrc
