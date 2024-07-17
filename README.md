# dotfiles (Mac mini)

## Setup

```
# change login shell & restart terminal
$ chsh -s /bin/bash

# generate ssh key & manually add it to github
$ ssh-keygen -t ed25519 -C "hacktk3@gmail.com"

# setup
$ /bin/bash -c "$(curl -L https://raw.githubusercontent.com/hacktk/dotfiles_macmini/main/setup.sh)"
```

## After setup

- Control Center
    - Sound - Always Show in Menu Bar
- Wallpaper
    - Colors - (Stone)
- Keyboard
    - Keyboard Shortcuts
        - Modifier Keys
            - Caps Lock (Change to Control)
            - Control (Change to Command)
            - Command (Change to Control)
    - Input Sources
        - Add
            - Alphanumeric (Google)
            - Hiragana (Google)
        - Delete
            - ABC (After check to `Romaji` in Japan - Romaji Input modes)
            - Japanese - Romaji
