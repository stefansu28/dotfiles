#!/bin/bash

set -e

script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $script_dir

delete_and_link() {
    local link_path=$1
    local dest_path=$2
    rm -rf $link_path
    ln -s $script_dir/$dest_path $link_path
}

# only delete and link files that exist in the repo
link_dir() {
    local link_path=$1
    local dest_path=$2
    for file in `ls ${dest_path}`; do
        echo "linking: $link_path/$file $dest_path/$file"
        delete_and_link "$link_path/$file" "$dest_path/$file"
    done
}

exists() {
    command -v "$1" >/dev/null 2>&1
}

# emacs
echo "Initializing emacs..."
link_dir ~/.emacs.d emacs.d

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


if exists i3; then
  echo "Initializing i3..."
  link_dir ~/.i3 i3
fi
