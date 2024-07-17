#!/bin/bash

set -eux

DOT_PATH="$HOME/repo/dotfiles_macmini"

if [ ! -d "$DOT_PATH" ]; then
    echo "Error: Directory $DOT_PATH does not exist."
    exit 1
fi
cd "$DOT_PATH" || exit 1

# dotfiles
for file in .??*; do
    [[ "$file" = ".git" ]] && continue
    [[ "$file" = ".DS_Store" ]] && continue
    ln -fvs "$DOT_PATH/$file" "$HOME/$file"
done

# git
mkdir -p "$HOME/.config"
ln -fvs "$DOT_PATH/git" "$HOME/.config/git"

# visual-studio-code
VSC_PATH="$HOME/Library/Application Support/Code/User"
ln -fvs "$DOT_PATH/vscode/settings.json" "$VSC_PATH/settings.json"
ln -fvs "$DOT_PATH/vscode/keybindings.json" "$VSC_PATH/keybindings.json"
