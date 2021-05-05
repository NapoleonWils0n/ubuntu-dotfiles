# ~/.zshenv

# set emacsclient as editor
export ALTERNATE_EDITOR=""
export EDITOR="/usr/bin/emacsclient -a emacs"
export VISUAL="/usr/bin/emacsclient -c -a emacs"

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
