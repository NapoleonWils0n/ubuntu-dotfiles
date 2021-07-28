# ~/.zshrc

# ssh zsh fix
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# fpath - commented out as it slows down shell start up
# fpath=($HOME/.zsh-completions $fpath)

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
#HISTFILE=~/.zsh_history

# set PATH so it includes user bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user local bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# git prompt script
if [ -f "$HOME/.git-prompt.sh" ]; then
	source "$HOME/.git-prompt.sh"
fi

# git prompt variables
setopt prompt_subst
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWUPSTREAM="auto verbose name git"

# right prompt git status
PROMPT=$'[%n@%M %~]'
RPROMPT='%F{cyan}$(__git_ps1 "%s")%f'

# variables for PS3 prompt
newline=$'\n'
yesmaster=' Yes Master ? '

# PS3 prompt function
function zle-line-init zle-keymap-select {
    VIM_NORMAL_PROMPT="[% -n]% "
    VIM_INSERT_PROMPT="[% +i]% "
    PS1="[%n@%M %~]${newline}${${KEYMAP/vicmd/$VIM_NORMAL_PROMPT}/(main|viins)/$VIM_INSERT_PROMPT}${yesmaster}"
    zle reset-prompt
}

# run PS3 prompt function
zle -N zle-line-init
zle -N zle-keymap-select

# set terminal window title to program name
case $TERM in
  (*xterm* | rxvt | rxvt-unicode-256color | st-256color)
    function precmd {
      print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
    }
    function preexec {
      printf "\033]0;%s\a" "$1"
    }
  ;;
esac

# XDG_RUNTIME_DIR for mpv hardware accleration
if [ -z "$XDG_RUNTIME_DIR" ]; then
    export XDG_RUNTIME_DIR=/tmp
    if [ ! -d  "$XDG_RUNTIME_DIR" ]; then
        mkdir "$XDG_RUNTIME_DIR"
        chmod 0700 "$XDG_RUNTIME_DIR"
    fi
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Fix bugs when switching modes
bindkey -v # vi mode
bindkey "^?" backward-delete-char
bindkey "^u" backward-kill-line
bindkey "^a" beginning-of-line
bindkey "^e" end-of-line
bindkey "^k" kill-line

# Use modern completion system
autoload -Uz compinit
compinit

# Set/unset  shell options
setopt notify globdots correct pushdtohome cdablevars autolist
setopt correctall recexact longlistjobs
setopt autoresume histignoredups pushdsilent noclobber
setopt autopushd pushdminus extendedglob rcquotes mailwarning
setopt histignorealldups sharehistory
#setopt auto_cd
cdpath=($HOME)
unsetopt bgnice autoparamslash

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

#eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# kill - red, green, blue
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=22=31=34'

# list optiones colour, white + cyan
zstyle ':completion:*:options' list-colors '=(#b) #(-[a-zA-Z0-9,]#)*(-- *)=36=37'

# rehash commands
zstyle ':completion:*' rehash true

# highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_STYLES[suffix-alias]=fg=cyan,underline
ZSH_HIGHLIGHT_STYLES[precommand]=fg=cyan,underline
ZSH_HIGHLIGHT_STYLES[arg0]=fg=cyan
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
ZSH_HIGHLIGHT_PATTERNS=('rm -rf *' 'fg=white,bold,bg=red')

# aliases

# hdmi display on
alias hdmi-on='xrandr --output eDP-1-1 --auto --primary --output DP-1-3 --mode 1920x1080 --right-of eDP-1-1 && ~/.fehbg &>/dev/null'

# hdmi display off
alias hdmi-off='xrandr --output eDP-1-1 --auto --primary --output DP-1-3 --off && ~/.fehbg &>/dev/null'

# functions

# transmission

# clear completed torrents
trd-clearcompleted() {
  transmission-remote -l | grep 100% | grep Done | \
  awk '{print $1}' | xargs -n 1 -I % transmission-remote -t % -r
}

trd-altdownloadspeed() { transmission-remote --downlimit "${@:-900}" ;}	# download default to 900K, else enter your own
trd-altdownloadspeedunlimited() { transmission-remote --no-downlimit ;}
trd-limitupload() { transmission-remote --uplimit "${@:-10}" ;}	# upload default to 10kpbs, else enter your own
trd-limituploadunlimited() { transmission-remote --no-uplimit ;}
trd-askmorepeers() { transmission-remote -t"$1" --reannounce ;}
trd-daemon() { transmission-daemon ;}
trd-quit() { killall transmission-daemon ;}
trd-add() { transmission-remote --add "$1" ;}
trd-hash() { transmission-remote --add "magnet:?xt=urn:btih:$1" ;}       # adding via hash info
trd-verify() { transmission-remote --verify "$1" ;}
trd-pause() { transmission-remote -t"$1" --stop ;}		# <id> or all
trd-start() { transmission-remote -t"$1" --start ;}		# <id> or all
trd-purge() { transmission-remote -t"$1" --remove-and-delete ;} # delete data also
trd-remove() { transmission-remote -t"$1" --remove ;}		# leaves data alone
trd-info() { transmission-remote -t"$1" --info ;}
trd-speed() { while true;do clear; transmission-remote -t"$1" -i | grep Speed;sleep 1;done ;}
trd-grep() { transmission-remote --list | grep -i "$1" ;}
trd-list() { transmission-remote --list ;}
trd-show() { transmission-show "$1" ;}                          # show .torrent file information

# namespace autocomplete
compdef _precommand namespace
