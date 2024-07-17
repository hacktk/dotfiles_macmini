#!/bin/bash

set -eux

# install brew & command line tools
if ! command -v brew > /dev/null 2>&1; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    sudo softwareupdate --install-rosetta
    echo
fi
eval "$(/opt/homebrew/bin/brew shellenv)"

# repository download
DOT_PATH="$HOME/repo/dotfiles_macmini"
if [ ! -d "$DOT_PATH" ]; then
    git clone git@github.com:hacktk/dotfiles_macmini.git "$DOT_PATH"
else
    echo "$DOT_PATH already downloaded. Updating..."
    cd "$DOT_PATH"
    git stash
    git checkout main
    git pull origin main
    echo
fi
cd "$DOT_PATH"

# install packages
./scripts/install.sh
echo

# deploy dotfiles
./scripts/deploy.sh
echo

# configure environments
./scripts/configure.sh
echo

echo "setup finished."
