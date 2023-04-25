# ~/.zshenv

# Path
typeset -U PATH path
path=("$HOME/bin" "$path[@]")
export PATH

# xdg directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# ssh-add
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

# less
export LESSHISTFILE="${XDG_CONFIG_HOME}/less/history"
export LESSKEY="${XDG_CONFIG_HOME}/less/keys"

# wget
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"

# libdvdcss
export DVDCSS_CACHE="${XDG_DATA_HOME}/dvdcss"

# set emacsclient as editor
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient -a emacs"

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
export GIT_PAGER="bat --color=always -p -l rs"
