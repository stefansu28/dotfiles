#!/bin/bash

set -e

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $script_dir

delete_and_link() {
    link_path=$1
    dest_path=$2
    rm -rf $link_path
    ln -s $script_dir/$dest_path $link_path
}

exists()
{
    command -v "$1" >/dev/null 2>&1
}

# emacs
echo "Initializing emacs..."
delete_and_link ~/.emacs.d/init.el emacs.d/init.el

# TODO zsh + bash
# echo "Initializing fish..."
# delete_and_link ~/.zsh zsh
# echo "  You'll need to install zsh and set it as your shell manually"

# Git TODO
# echo "Initializing git..."
# delete_and_link ~/.gitconfig gitconfig

# # asciiArt
# echo "Initializing asciiArt..."
# delete_and_link ~/asciiArt asciiArt


# TODO
# if exists i3; then
#   echo "Initializing i3..."
#   delete_and_link ~/.i3/config i3/config
# fi
