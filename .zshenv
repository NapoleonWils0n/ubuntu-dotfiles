# ~/.zshenv

# Path
typeset -U PATH path
path=("$HOME/bin" "$HOME/.local/bin" "$path[@]")
export PATH

# xdg directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

#  less
export LESSHISTFILE="${XDG_CONFIG_HOME}/less/history"
export LESSKEY="${XDG_CONFIG_HOME}/less/keys"

# set emacsclient as editor
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient -c -a emacs"

# ssh-add
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# tell ls to be colourfull
export LSCOLORS=ExFxCxDxBxegedabagacad
export CLICOLOR=1

# qt5
export QT_QPA_PLATFORMTHEME=qt5ct

# vi mode
export KEYTIMEOUT=1

# mpd host variable for mpc
export MPD_HOST="/home/djwilcox/.config/mpd/socket"

# git pager bat with colour
export GIT_PAGER="bat --color=always -l rs"


# export fix for nixpkgs
export LC_ALL=C

# nix os xdg directories
export XDG_DATA_DIRS="$HOME/.nix-profile/share:$XDG_DATA_DIRS"

# nixpkgs
if [ -e /home/djwilcox/.nix-profile/etc/profile.d/nix.sh ]; then . /home/djwilcox/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
