#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/local/etc/X11/xinit/.Xresources
sysmodmap=/usr/local/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /usr/local/etc/X11/xinit/xinitrc.d ] ; then
	for f in /usr/local/etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

# remap ctrl to alt, alt to super, super to ctrl
xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/keymap.xkb $DISPLAY

# feh set wallpaper
feh --no-fehbg --bg-center --image-bg '#002b36' '/home/djwilcox/.wallpaper/linux.png' 

# start ssh-agent and window mamager
#exec ssh-agent /usr/local/bin/i3
# start slstatus bar
/usr/local/bin/slstatus &
exec ssh-agent /usr/local/bin/dwm
